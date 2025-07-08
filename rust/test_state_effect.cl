; Test State effects
(do
  (effect state:set "mykey" "myvalue")
  (effect state:get "mykey"))