// Flexbox layout algorithm implementation
// Based on CSS Flexbox specification

use super::base::{ComponentId, LayoutConstraints, LayoutSize};
use super::style::{FlexDirection, JustifyContent, AlignItems, AlignSelf};
use cgmath::Vector2;

/// Flex item data during layout calculation
#[derive(Debug, Clone)]
pub struct FlexItem {
    /// Component ID
    pub id: ComponentId,
    /// Natural size when unconstrained
    pub natural_size: LayoutSize,
    /// Final computed size
    pub computed_size: LayoutSize,
    /// Position relative to container
    pub position: Vector2<f32>,
    /// Flex grow factor
    pub flex_grow: f32,
    /// Flex shrink factor
    pub flex_shrink: f32,
    /// Flex basis
    pub flex_basis: Option<f32>,
    /// Align self override
    pub align_self: AlignSelf,
    /// Margins
    pub margin: (f32, f32, f32, f32), // top, right, bottom, left
}

/// Flexbox layout calculator
pub struct FlexboxLayout {
    /// Container direction
    direction: FlexDirection,
    /// Main axis alignment
    justify_content: JustifyContent,
    /// Cross axis alignment
    align_items: AlignItems,
    /// Gap between items
    gap: f32,
    /// Whether to wrap items
    flex_wrap: bool,
}

impl FlexboxLayout {
    pub fn new(direction: FlexDirection) -> Self {
        Self {
            direction,
            justify_content: JustifyContent::Start,
            align_items: AlignItems::Stretch,
            gap: 0.0,
            flex_wrap: false,
        }
    }
    
    pub fn with_justify(mut self, justify: JustifyContent) -> Self {
        self.justify_content = justify;
        self
    }
    
    pub fn with_align(mut self, align: AlignItems) -> Self {
        self.align_items = align;
        self
    }
    
    pub fn with_gap(mut self, gap: f32) -> Self {
        self.gap = gap;
        self
    }
    
    pub fn with_wrap(mut self, wrap: bool) -> Self {
        self.flex_wrap = wrap;
        self
    }
    
    /// Calculate layout for flex items
    pub fn calculate_layout(
        &self,
        items: &mut [FlexItem],
        constraints: &LayoutConstraints,
        padding: (f32, f32, f32, f32), // top, right, bottom, left
    ) -> LayoutSize {
        if items.is_empty() {
            return LayoutSize {
                width: padding.1 + padding.3,
                height: padding.0 + padding.2,
            };
        }
        
        // Available space for content (excluding padding)
        let available_width = constraints.available_size.x - padding.1 - padding.3;
        let available_height = constraints.available_size.y - padding.0 - padding.2;
        
        // Determine main and cross axis sizes
        let (main_size, cross_size) = match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => (available_width, available_height),
            FlexDirection::Column | FlexDirection::ColumnReverse => (available_height, available_width),
        };
        
        // Step 1: Calculate natural sizes for all items
        let mut total_main_size = 0.0;
        let mut max_cross_size = 0.0;
        let mut total_grow = 0.0;
        let mut total_shrink = 0.0;
        
        for item in items.iter_mut() {
            let item_main = self.get_main_size(&item.natural_size);
            let item_cross = self.get_cross_size(&item.natural_size);
            
            total_main_size += item_main;
            max_cross_size = f32::max(max_cross_size, item_cross);
            total_grow += item.flex_grow;
            total_shrink += item.flex_shrink;
        }
        
        // Add gaps to total
        let gap_total = self.gap * (items.len() - 1) as f32;
        total_main_size += gap_total;
        
        // Step 2: Distribute space along main axis
        let remaining_space = main_size - total_main_size;
        
        if remaining_space > 0.0 && total_grow > 0.0 {
            // Distribute positive free space
            let grow_unit = remaining_space / total_grow;
            
            for item in items.iter_mut() {
                if item.flex_grow > 0.0 {
                    let growth = grow_unit * item.flex_grow;
                    self.add_main_size(&mut item.computed_size, &item.natural_size, growth);
                } else {
                    item.computed_size = item.natural_size;
                }
            }
        } else if remaining_space < 0.0 && total_shrink > 0.0 {
            // Distribute negative free space
            let shrink_unit = -remaining_space / total_shrink;
            
            for item in items.iter_mut() {
                if item.flex_shrink > 0.0 {
                    let shrinkage = shrink_unit * item.flex_shrink;
                    self.subtract_main_size(&mut item.computed_size, &item.natural_size, shrinkage);
                } else {
                    item.computed_size = item.natural_size;
                }
            }
        } else {
            // No flex distribution needed
            for item in items.iter_mut() {
                item.computed_size = item.natural_size;
            }
        }
        
        // Step 3: Align items on cross axis
        for item in items.iter_mut() {
            let align = if item.align_self == AlignSelf::Auto {
                self.align_items
            } else {
                match item.align_self {
                    AlignSelf::Start => AlignItems::Start,
                    AlignSelf::End => AlignItems::End,
                    AlignSelf::Center => AlignItems::Center,
                    AlignSelf::Stretch => AlignItems::Stretch,
                    AlignSelf::Baseline => AlignItems::Baseline,
                    AlignSelf::Auto => unreachable!(),
                }
            };
            
            match align {
                AlignItems::Stretch => {
                    self.set_cross_size(&mut item.computed_size, cross_size);
                }
                _ => {} // Other alignments don't affect size
            }
        }
        
        // Step 4: Position items
        let mut main_pos = padding.3; // Start with left padding for row, top for column
        
        // Calculate total used space for justification
        let mut total_used = 0.0;
        for item in items.iter() {
            total_used += self.get_main_size(&item.computed_size);
        }
        total_used += gap_total;
        
        // Apply justification
        let free_space = main_size - total_used;
        let (start_offset, between_offset) = match self.justify_content {
            JustifyContent::Start => (0.0, 0.0),
            JustifyContent::End => (free_space, 0.0),
            JustifyContent::Center => (free_space / 2.0, 0.0),
            JustifyContent::SpaceBetween => {
                if items.len() > 1 {
                    (0.0, free_space / (items.len() - 1) as f32)
                } else {
                    (0.0, 0.0)
                }
            }
            JustifyContent::SpaceAround => {
                let space_unit = free_space / items.len() as f32;
                (space_unit / 2.0, space_unit)
            }
            JustifyContent::SpaceEvenly => {
                let space_unit = free_space / (items.len() + 1) as f32;
                (space_unit, space_unit)
            }
        };
        
        main_pos += start_offset;
        
        // Position each item
        let item_count = items.len();
        for (i, item) in items.iter_mut().enumerate() {
            let item_main_size = self.get_main_size(&item.computed_size);
            let item_cross_size = self.get_cross_size(&item.computed_size);
            
            // Cross axis position based on alignment
            let cross_pos = match self.align_items {
                AlignItems::Start => padding.0, // top padding
                AlignItems::End => cross_size - item_cross_size + padding.0,
                AlignItems::Center => (cross_size - item_cross_size) / 2.0 + padding.0,
                AlignItems::Stretch | AlignItems::Baseline => padding.0,
            };
            
            // Set position based on direction
            match self.direction {
                FlexDirection::Row => {
                    item.position = Vector2::new(main_pos, cross_pos);
                }
                FlexDirection::RowReverse => {
                    item.position = Vector2::new(main_size - main_pos - item_main_size + padding.3, cross_pos);
                }
                FlexDirection::Column => {
                    item.position = Vector2::new(cross_pos, main_pos);
                }
                FlexDirection::ColumnReverse => {
                    item.position = Vector2::new(cross_pos, main_size - main_pos - item_main_size + padding.0);
                }
            }
            
            main_pos += item_main_size;
            if i < item_count - 1 {
                main_pos += self.gap + between_offset;
            }
        }
        
        // Calculate final container size
        let content_width = match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => total_used,
            FlexDirection::Column | FlexDirection::ColumnReverse => max_cross_size,
        };
        
        let content_height = match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => max_cross_size,
            FlexDirection::Column | FlexDirection::ColumnReverse => total_used,
        };
        
        LayoutSize {
            width: content_width + padding.1 + padding.3,
            height: content_height + padding.0 + padding.2,
        }
    }
    
    // Helper methods for main/cross axis operations
    
    fn get_main_size(&self, size: &LayoutSize) -> f32 {
        match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => size.width,
            FlexDirection::Column | FlexDirection::ColumnReverse => size.height,
        }
    }
    
    fn get_cross_size(&self, size: &LayoutSize) -> f32 {
        match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => size.height,
            FlexDirection::Column | FlexDirection::ColumnReverse => size.width,
        }
    }
    
    fn add_main_size(&self, computed: &mut LayoutSize, natural: &LayoutSize, amount: f32) {
        match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => {
                computed.width = natural.width + amount;
                computed.height = natural.height;
            }
            FlexDirection::Column | FlexDirection::ColumnReverse => {
                computed.width = natural.width;
                computed.height = natural.height + amount;
            }
        }
    }
    
    fn subtract_main_size(&self, computed: &mut LayoutSize, natural: &LayoutSize, amount: f32) {
        match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => {
                computed.width = (natural.width - amount).max(0.0);
                computed.height = natural.height;
            }
            FlexDirection::Column | FlexDirection::ColumnReverse => {
                computed.width = natural.width;
                computed.height = (natural.height - amount).max(0.0);
            }
        }
    }
    
    fn set_cross_size(&self, size: &mut LayoutSize, cross: f32) {
        match self.direction {
            FlexDirection::Row | FlexDirection::RowReverse => {
                size.height = cross;
            }
            FlexDirection::Column | FlexDirection::ColumnReverse => {
                size.width = cross;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_flexbox_row_layout() {
        let mut layout = FlexboxLayout::new(FlexDirection::Row)
            .with_justify(JustifyContent::SpaceBetween)
            .with_gap(10.0);
        
        let mut items = vec![
            FlexItem {
                id: 1,
                natural_size: LayoutSize { width: 100.0, height: 50.0 },
                computed_size: LayoutSize { width: 0.0, height: 0.0 },
                position: Vector2::new(0.0, 0.0),
                flex_grow: 0.0,
                flex_shrink: 1.0,
                flex_basis: None,
                align_self: AlignSelf::Auto,
                margin: (0.0, 0.0, 0.0, 0.0),
            },
            FlexItem {
                id: 2,
                natural_size: LayoutSize { width: 150.0, height: 60.0 },
                computed_size: LayoutSize { width: 0.0, height: 0.0 },
                position: Vector2::new(0.0, 0.0),
                flex_grow: 0.0,
                flex_shrink: 1.0,
                flex_basis: None,
                align_self: AlignSelf::Auto,
                margin: (0.0, 0.0, 0.0, 0.0),
            },
        ];
        
        let constraints = LayoutConstraints::bounded(
            Vector2::new(0.0, 0.0),
            Vector2::new(400.0, 200.0)
        );
        
        let size = layout.calculate_layout(&mut items, &constraints, (0.0, 0.0, 0.0, 0.0));
        
        assert_eq!(items[0].position.x, 0.0);
        assert_eq!(items[1].position.x, 250.0); // 0 + 100 + 10 gap + 140 free space
        assert_eq!(size.width, 260.0); // 100 + 10 + 150
        assert_eq!(size.height, 60.0); // max height
    }
}