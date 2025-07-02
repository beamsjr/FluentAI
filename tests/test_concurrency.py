"""
Test concurrency primitives
"""

import unittest
import time
from src.interpreter.interpreter import Interpreter
from src.effects.handlers import create_test_handler


class TestConcurrency(unittest.TestCase):
    """Test concurrency primitives"""
    
    def setUp(self):
        self.handler = create_test_handler()
        self.interpreter = Interpreter(effect_handler=self.handler)
    
    def eval(self, code: str):
        """Helper to parse and evaluate code"""
        result = self.interpreter.eval(code)
        return result.data if hasattr(result, 'data') else result
    
    def test_channel_creation(self):
        """Test creating channels"""
        # Unbuffered channel
        code = "(chan)"
        result = self.eval(code)
        self.assertIsInstance(result, str)  # Channel ID
        
        # Buffered channel
        code = "(chan 10)"
        result = self.eval(code)
        self.assertIsInstance(result, str)
    
    def test_channel_send_receive(self):
        """Test sending and receiving on channels"""
        code = """
        (let ((ch (chan)))
          (do
            (send! ch 42)
            (let ((result (receive! ch)))
              (get result "value"))))
        """
        result = self.eval(code)
        self.assertEqual(result, 42)
    
    def test_buffered_channel(self):
        """Test buffered channel operations"""
        code = """
        (let ((ch (chan 3)))
          (do
            (send! ch 1)
            (send! ch 2)
            (send! ch 3)
            (let ((r1 (receive! ch))
                  (r2 (receive! ch))
                  (r3 (receive! ch)))
              [(get r1 "value") (get r2 "value") (get r3 "value")])))
        """
        result = self.eval(code)
        self.assertEqual(result, [1, 2, 3])
    
    def test_goroutine(self):
        """Test launching goroutines"""
        code = """
        (let ((ch (chan)))
          (do
            (go (send! ch "Hello from goroutine"))
            (effect time:sleep 0.01)
            (let ((result (receive! ch)))
              (get result "value"))))
        """
        result = self.eval(code)
        self.assertEqual(result, "Hello from goroutine")
    
    def test_multiple_goroutines(self):
        """Test multiple goroutines"""
        code = """
        (let ((ch (chan 3)))
          (do
            (go (send! ch 1))
            (go (send! ch 2))
            (go (send! ch 3))
            (effect time:sleep 0.05)
            (sort [(get (receive! ch) "value")
                   (get (receive! ch) "value")
                   (get (receive! ch) "value")])))
        """
        result = self.eval(code)
        self.assertEqual(result, [1, 2, 3])
    
    def test_producer_consumer(self):
        """Test producer-consumer pattern"""
        # Simpler version without goroutines first
        code = """
        (let ((ch (chan 5)))
          (do
            ; Send values
            (map (lambda (i) (send! ch i)) [1 2 3 4 5])
            
            ; Collect values
            [(get (receive! ch) "value")
             (get (receive! ch) "value")
             (get (receive! ch) "value")
             (get (receive! ch) "value")
             (get (receive! ch) "value")]))
        """
        result = self.eval(code)
        self.assertEqual(result, [1, 2, 3, 4, 5])
    
    def test_channel_close(self):
        """Test closing channels"""
        code = """
        (let ((ch (chan)))
          (do
            (effect concurrent:close ch)
            (let ((result (receive! ch)))
              (get result "ok"))))
        """
        result = self.eval(code)
        self.assertFalse(result)
    
    def test_mutex(self):
        """Test mutex for synchronization"""
        code = """
        (let ((mu (effect concurrent:mutex))
              (ch (chan 3)))
          (do
            ; Launch multiple goroutines that send values
            (let ((inc (lambda (id)
                        (do
                          (effect concurrent:lock mu)
                          (send! ch id)
                          (effect concurrent:unlock mu)))))
              (do
                (go (inc 1))
                (go (inc 2))
                (go (inc 3))
                (effect time:sleep 0.05)
                ; Collect all values
                (sort [(get (receive! ch) "value")
                       (get (receive! ch) "value")
                       (get (receive! ch) "value")])))))
        """
        result = self.eval(code)
        self.assertEqual(result, [1, 2, 3])
    
    def test_semaphore(self):
        """Test semaphore"""
        code = """
        (let ((sem (effect concurrent:semaphore 2))
              (active-ch (chan 10))
              (done-ch (chan 4)))
          (do
            (let ((worker (lambda (id)
                           (do
                             (effect concurrent:acquire sem)
                             (send! active-ch "start")
                             (effect time:sleep 0.01)
                             (send! active-ch "end")
                             (effect concurrent:release sem)
                             (send! done-ch id)))))
              (do
                (go (worker 1))
                (go (worker 2))
                (go (worker 3))
                (go (worker 4))
                ; Wait for all workers to complete
                (map (lambda (_) (receive! done-ch)) [1 2 3 4])
                ; Count max concurrent active
                (let ((count-active (lambda (events active max-active)
                                     (if (null? events)
                                         max-active
                                         (let ((event (car events))
                                               (rest (cdr events)))
                                           (if (= event "start")
                                               (let ((new-active (+ active 1)))
                                                 (count-active rest new-active 
                                                              (if (> new-active max-active)
                                                                  new-active
                                                                  max-active)))
                                               (count-active rest (- active 1) max-active)))))))
                  ; Collect all events
                  (let ((collect-events (lambda (n acc)
                                         (if (= n 0)
                                             acc
                                             (let ((r (receive! active-ch)))
                                               (if (get r "ok")
                                                   (collect-events (- n 1) (cons (get r "value") acc))
                                                   acc))))))
                    (let ((events (reverse (collect-events 8 []))))
                      (count-active events 0 0)))))))
        """
        result = self.eval(code)
        self.assertLessEqual(result, 2)  # At most 2 workers active
    
    def test_barrier(self):
        """Test barrier synchronization"""
        code = """
        (let ((barrier (effect concurrent:barrier 3))
              (ch (chan 3)))
          (do
            (let ((worker (lambda (id)
                           (do
                             (effect time:sleep (* id 0.01))
                             (effect concurrent:wait barrier)
                             (send! ch id)))))
              (do
                (go (worker 1))
                (go (worker 2))
                (go (worker 3))
                (effect time:sleep 0.05)
                ; All should arrive at roughly the same time
                (let ((r1 (get (receive! ch) "value"))
                      (r2 (get (receive! ch) "value"))
                      (r3 (get (receive! ch) "value")))
                  (sort [r1 r2 r3]))))))
        """
        result = self.eval(code)
        self.assertEqual(result, [1, 2, 3])
    
    def test_concurrent_map(self):
        """Test concurrent map operation"""
        code = """
        (let ((concurrent-map (lambda (f items)
                               (let ((ch (chan (length items))))
                                 (do
                                   ; Launch goroutines
                                   (map (lambda (item)
                                         (go (send! ch (f item))))
                                       items)
                                   ; Collect results
                                   (map (lambda (_)
                                         (get (receive! ch) "value"))
                                       items))))))
          (concurrent-map (lambda (x) (* x x)) [1 2 3 4 5]))
        """
        result = self.eval(code)
        self.assertEqual(sorted(result), [1, 4, 9, 16, 25])
    
    def test_fan_out_fan_in(self):
        """Test fan-out/fan-in pattern"""
        code = """
        (let ((in-ch (chan))
              (out-ch (chan)))
          (do
            ; Fan-out: multiple workers
            (let ((worker (lambda (id)
                           (let ((loop (lambda ()
                                        (let ((r (receive! in-ch)))
                                          (if (get r "ok")
                                              (do
                                                (send! out-ch (* (get r "value") (get r "value")))
                                                (loop))
                                              nil))))))
                     (loop))))
              (do
                (go (worker 1))
                (go (worker 2))
                (go (worker 3))))
            
            ; Send work
            (map (lambda (i) (send! in-ch i)) [1 2 3 4 5])
            
            ; Collect results
            (let ((collect (lambda (n acc)
                            (if (= n 0)
                                (sort acc)
                                (let ((r (receive! out-ch)))
                                  (collect (- n 1) (cons (get r "value") acc)))))))
              (collect 5 []))))
        """
        result = self.eval(code)
        self.assertEqual(result, [1, 4, 9, 16, 25])


if __name__ == "__main__":
    unittest.main()