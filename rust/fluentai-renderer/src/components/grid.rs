// CSS Grid layout implementation for FluentAI

use super::base::{ComponentId, LayoutConstraints, LayoutSize};
use cgmath::Vector2;
use std::collections::HashMap;

/// Grid template definition for rows or columns
#[derive(Debug, Clone)]
pub enum GridTrackSize {
    /// Fixed pixel size
    Pixels(f32),
    /// Fraction of available space
    Fraction(f32),
    /// Auto-size based on content
    Auto,
    /// Minimum and maximum size
    MinMax(Box<GridTrackSize>, Box<GridTrackSize>),
    /// Fit content with optional maximum
    FitContent(Option<f32>),
}

/// Grid line name for named grid lines
pub type GridLineName = String;

/// Grid template for rows or columns
#[derive(Debug, Clone)]
pub struct GridTemplate {
    /// Track sizes
    pub tracks: Vec<GridTrackSize>,
    /// Named grid lines
    pub line_names: HashMap<usize, Vec<GridLineName>>,
}

impl GridTemplate {
    pub fn new() -> Self {
        Self {
            tracks: Vec::new(),
            line_names: HashMap::new(),
        }
    }
    
    /// Parse from string like "100px 1fr 2fr auto"
    pub fn from_string(template: &str) -> Self {
        let mut tracks = Vec::new();
        let parts = template.split_whitespace();
        
        for part in parts {
            if let Some(track) = Self::parse_track_size(part) {
                tracks.push(track);
            }
        }
        
        Self {
            tracks,
            line_names: HashMap::new(),
        }
    }
    
    fn parse_track_size(s: &str) -> Option<GridTrackSize> {
        if s == "auto" {
            Some(GridTrackSize::Auto)
        } else if let Some(px) = s.strip_suffix("px") {
            px.parse::<f32>().ok().map(GridTrackSize::Pixels)
        } else if let Some(fr) = s.strip_suffix("fr") {
            fr.parse::<f32>().ok().map(GridTrackSize::Fraction)
        } else {
            None
        }
    }
}

/// Grid item placement
#[derive(Debug, Clone)]
pub struct GridPlacement {
    /// Row start position (1-indexed)
    pub row_start: i32,
    /// Row end position (exclusive)
    pub row_end: Option<i32>,
    /// Column start position (1-indexed)
    pub column_start: i32,
    /// Column end position (exclusive)
    pub column_end: Option<i32>,
}

impl Default for GridPlacement {
    fn default() -> Self {
        Self {
            row_start: -1, // Auto placement
            row_end: None,
            column_start: -1, // Auto placement
            column_end: None,
        }
    }
}

/// Grid gap (row and column gaps)
#[derive(Debug, Clone, Copy)]
pub struct GridGap {
    pub row_gap: f32,
    pub column_gap: f32,
}

impl GridGap {
    pub fn uniform(gap: f32) -> Self {
        Self {
            row_gap: gap,
            column_gap: gap,
        }
    }
}

/// Grid alignment options
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum GridAlign {
    Start,
    End,
    Center,
    Stretch,
    SpaceBetween,
    SpaceAround,
    SpaceEvenly,
}

/// Grid item data during layout
#[derive(Debug, Clone)]
pub struct GridItem {
    /// Component ID
    pub id: ComponentId,
    /// Natural size when unconstrained
    pub natural_size: LayoutSize,
    /// Final computed size
    pub computed_size: LayoutSize,
    /// Position relative to container
    pub position: Vector2<f32>,
    /// Grid placement
    pub placement: GridPlacement,
    /// Item-specific alignment
    pub align_self: Option<GridAlign>,
    pub justify_self: Option<GridAlign>,
}

/// Grid layout calculator
pub struct GridLayout {
    /// Row template
    row_template: GridTemplate,
    /// Column template
    column_template: GridTemplate,
    /// Row/column gaps
    gap: GridGap,
    /// Alignment for items
    align_items: GridAlign,
    justify_items: GridAlign,
    /// Alignment for content
    align_content: GridAlign,
    justify_content: GridAlign,
    /// Auto flow direction
    auto_flow: GridAutoFlow,
}

#[derive(Debug, Clone, Copy)]
pub enum GridAutoFlow {
    Row,
    Column,
    RowDense,
    ColumnDense,
}

impl GridLayout {
    pub fn new() -> Self {
        Self {
            row_template: GridTemplate::new(),
            column_template: GridTemplate::new(),
            gap: GridGap::uniform(0.0),
            align_items: GridAlign::Stretch,
            justify_items: GridAlign::Stretch,
            align_content: GridAlign::Start,
            justify_content: GridAlign::Start,
            auto_flow: GridAutoFlow::Row,
        }
    }
    
    pub fn with_rows(mut self, template: GridTemplate) -> Self {
        self.row_template = template;
        self
    }
    
    pub fn with_columns(mut self, template: GridTemplate) -> Self {
        self.column_template = template;
        self
    }
    
    pub fn with_gap(mut self, gap: GridGap) -> Self {
        self.gap = gap;
        self
    }
    
    pub fn with_align_items(mut self, align: GridAlign) -> Self {
        self.align_items = align;
        self
    }
    
    pub fn with_justify_items(mut self, justify: GridAlign) -> Self {
        self.justify_items = justify;
        self
    }
    
    /// Calculate layout for grid items
    pub fn calculate_layout(
        &self,
        items: &mut [GridItem],
        constraints: &LayoutConstraints,
        padding: (f32, f32, f32, f32), // top, right, bottom, left
    ) -> LayoutSize {
        let available_width = constraints.available_size.x - padding.1 - padding.3;
        let available_height = constraints.available_size.y - padding.0 - padding.2;
        
        // Step 1: Calculate track sizes
        let (row_sizes, row_positions) = self.calculate_track_sizes(
            &self.row_template,
            available_height,
            self.gap.row_gap,
            true,
            items,
        );
        
        let (col_sizes, col_positions) = self.calculate_track_sizes(
            &self.column_template,
            available_width,
            self.gap.column_gap,
            false,
            items,
        );
        
        // Step 2: Auto-place items that don't have explicit placement
        self.auto_place_items(items, row_sizes.len(), col_sizes.len());
        
        // Step 3: Position and size each item
        for item in items.iter_mut() {
            // Determine grid area
            let row_start = (item.placement.row_start as usize).saturating_sub(1);
            let row_end = item.placement.row_end
                .map(|e| (e as usize).saturating_sub(1))
                .unwrap_or(row_start + 1)
                .min(row_sizes.len());
            
            let col_start = (item.placement.column_start as usize).saturating_sub(1);
            let col_end = item.placement.column_end
                .map(|e| (e as usize).saturating_sub(1))
                .unwrap_or(col_start + 1)
                .min(col_sizes.len());
            
            // Calculate cell area
            if row_start < row_positions.len() && col_start < col_positions.len() {
                let cell_x = col_positions[col_start] + padding.3;
                let cell_y = row_positions[row_start] + padding.0;
                
                let cell_width = if col_end > col_start + 1 {
                    // Span multiple columns
                    let mut width = 0.0;
                    for i in col_start..col_end {
                        width += col_sizes.get(i).unwrap_or(&0.0);
                        if i < col_end - 1 {
                            width += self.gap.column_gap;
                        }
                    }
                    width
                } else {
                    *col_sizes.get(col_start).unwrap_or(&0.0)
                };
                
                let cell_height = if row_end > row_start + 1 {
                    // Span multiple rows
                    let mut height = 0.0;
                    for i in row_start..row_end {
                        height += row_sizes.get(i).unwrap_or(&0.0);
                        if i < row_end - 1 {
                            height += self.gap.row_gap;
                        }
                    }
                    height
                } else {
                    *row_sizes.get(row_start).unwrap_or(&0.0)
                };
                
                // Apply alignment
                let align = item.align_self.unwrap_or(self.align_items);
                let justify = item.justify_self.unwrap_or(self.justify_items);
                
                let (item_width, item_x_offset) = match justify {
                    GridAlign::Start => (item.natural_size.width.min(cell_width), 0.0),
                    GridAlign::End => (item.natural_size.width.min(cell_width), cell_width - item.natural_size.width),
                    GridAlign::Center => (item.natural_size.width.min(cell_width), (cell_width - item.natural_size.width) / 2.0),
                    GridAlign::Stretch => (cell_width, 0.0),
                    _ => (item.natural_size.width.min(cell_width), 0.0),
                };
                
                let (item_height, item_y_offset) = match align {
                    GridAlign::Start => (item.natural_size.height.min(cell_height), 0.0),
                    GridAlign::End => (item.natural_size.height.min(cell_height), cell_height - item.natural_size.height),
                    GridAlign::Center => (item.natural_size.height.min(cell_height), (cell_height - item.natural_size.height) / 2.0),
                    GridAlign::Stretch => (cell_height, 0.0),
                    _ => (item.natural_size.height.min(cell_height), 0.0),
                };
                
                item.computed_size = LayoutSize {
                    width: item_width,
                    height: item_height,
                };
                
                item.position = Vector2::new(
                    cell_x + item_x_offset,
                    cell_y + item_y_offset,
                );
            }
        }
        
        // Calculate total grid size
        let total_width = col_positions.last().unwrap_or(&0.0) + col_sizes.last().unwrap_or(&0.0) + padding.1 + padding.3;
        let total_height = row_positions.last().unwrap_or(&0.0) + row_sizes.last().unwrap_or(&0.0) + padding.0 + padding.2;
        
        LayoutSize {
            width: total_width,
            height: total_height,
        }
    }
    
    /// Calculate track sizes for rows or columns
    fn calculate_track_sizes(
        &self,
        template: &GridTemplate,
        available_space: f32,
        gap: f32,
        is_row: bool,
        items: &[GridItem],
    ) -> (Vec<f32>, Vec<f32>) {
        if template.tracks.is_empty() {
            // Auto-generate tracks based on items
            let max_track = items.iter()
                .map(|item| {
                    if is_row {
                        item.placement.row_end.unwrap_or(item.placement.row_start + 1)
                    } else {
                        item.placement.column_end.unwrap_or(item.placement.column_start + 1)
                    }
                })
                .max()
                .unwrap_or(1);
            
            let count = max_track.max(1) as usize;
            let size = (available_space - gap * (count - 1) as f32) / count as f32;
            let sizes = vec![size; count];
            
            let mut positions = vec![0.0];
            for i in 1..count {
                positions.push(positions[i - 1] + sizes[i - 1] + gap);
            }
            
            return (sizes, positions);
        }
        
        // Calculate fixed and auto sizes first
        let mut sizes = vec![0.0; template.tracks.len()];
        let mut flexible_tracks = Vec::new();
        let total_gaps = gap * (template.tracks.len().saturating_sub(1)) as f32;
        let mut used_space = total_gaps;
        
        for (i, track) in template.tracks.iter().enumerate() {
            match track {
                GridTrackSize::Pixels(px) => {
                    sizes[i] = *px;
                    used_space += px;
                }
                GridTrackSize::Auto => {
                    // Calculate based on content
                    let content_size = items.iter()
                        .filter(|item| {
                            let track_match = if is_row {
                                item.placement.row_start as usize == i + 1
                            } else {
                                item.placement.column_start as usize == i + 1
                            };
                            track_match
                        })
                        .map(|item| {
                            if is_row {
                                item.natural_size.height
                            } else {
                                item.natural_size.width
                            }
                        })
                        .max_by(|a, b| a.partial_cmp(b).unwrap())
                        .unwrap_or(0.0);
                    
                    sizes[i] = content_size;
                    used_space += content_size;
                }
                GridTrackSize::Fraction(fr) => {
                    flexible_tracks.push((i, *fr));
                }
                _ => {} // TODO: Handle other track types
            }
        }
        
        // Distribute remaining space to flexible tracks
        if !flexible_tracks.is_empty() && available_space > used_space {
            let remaining = available_space - used_space;
            let total_fractions: f32 = flexible_tracks.iter().map(|(_, fr)| fr).sum();
            
            for (i, fr) in flexible_tracks {
                sizes[i] = (remaining * fr) / total_fractions;
            }
        }
        
        // Calculate positions
        let mut positions = vec![0.0];
        for i in 1..sizes.len() {
            positions.push(positions[i - 1] + sizes[i - 1] + gap);
        }
        
        (sizes, positions)
    }
    
    /// Auto-place items that don't have explicit placement
    fn auto_place_items(&self, items: &mut [GridItem], rows: usize, cols: usize) {
        let mut grid = vec![vec![false; cols]; rows];
        
        // Mark explicitly placed items
        for item in items.iter() {
            if item.placement.row_start > 0 && item.placement.column_start > 0 {
                let row = (item.placement.row_start as usize).saturating_sub(1);
                let col = (item.placement.column_start as usize).saturating_sub(1);
                
                if row < rows && col < cols {
                    grid[row][col] = true;
                }
            }
        }
        
        // Auto-place remaining items
        let mut current_row = 0;
        let mut current_col = 0;
        
        for item in items.iter_mut() {
            if item.placement.row_start <= 0 || item.placement.column_start <= 0 {
                // Find next available cell
                while current_row < rows && grid[current_row][current_col] {
                    current_col += 1;
                    if current_col >= cols {
                        current_col = 0;
                        current_row += 1;
                    }
                }
                
                if current_row < rows {
                    item.placement.row_start = (current_row + 1) as i32;
                    item.placement.column_start = (current_col + 1) as i32;
                    grid[current_row][current_col] = true;
                    
                    // Move to next position
                    current_col += 1;
                    if current_col >= cols {
                        current_col = 0;
                        current_row += 1;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_grid_template_parsing() {
        let template = GridTemplate::from_string("100px 1fr 2fr auto");
        assert_eq!(template.tracks.len(), 4);
        
        match &template.tracks[0] {
            GridTrackSize::Pixels(px) => assert_eq!(*px, 100.0),
            _ => panic!("Expected pixels"),
        }
        
        match &template.tracks[1] {
            GridTrackSize::Fraction(fr) => assert_eq!(*fr, 1.0),
            _ => panic!("Expected fraction"),
        }
    }
    
    #[test]
    fn test_grid_layout() {
        let mut layout = GridLayout::new()
            .with_columns(GridTemplate::from_string("100px 200px"))
            .with_rows(GridTemplate::from_string("50px 50px"))
            .with_gap(GridGap::uniform(10.0));
        
        let mut items = vec![
            GridItem {
                id: 1,
                natural_size: LayoutSize { width: 80.0, height: 40.0 },
                computed_size: LayoutSize { width: 0.0, height: 0.0 },
                position: Vector2::new(0.0, 0.0),
                placement: GridPlacement {
                    row_start: 1,
                    row_end: None,
                    column_start: 1,
                    column_end: None,
                },
                align_self: None,
                justify_self: None,
            },
            GridItem {
                id: 2,
                natural_size: LayoutSize { width: 150.0, height: 40.0 },
                computed_size: LayoutSize { width: 0.0, height: 0.0 },
                position: Vector2::new(0.0, 0.0),
                placement: GridPlacement {
                    row_start: 1,
                    row_end: None,
                    column_start: 2,
                    column_end: None,
                },
                align_self: None,
                justify_self: None,
            },
        ];
        
        let constraints = LayoutConstraints::bounded(
            Vector2::new(0.0, 0.0),
            Vector2::new(400.0, 200.0)
        );
        
        let size = layout.calculate_layout(&mut items, &constraints, (0.0, 0.0, 0.0, 0.0));
        
        // First item should be at (0, 0)
        assert_eq!(items[0].position.x, 0.0);
        assert_eq!(items[0].position.y, 0.0);
        
        // Second item should be at (110, 0) - 100px + 10px gap
        assert_eq!(items[1].position.x, 110.0);
        assert_eq!(items[1].position.y, 0.0);
        
        // Total size should be 310x110 (100 + 10 + 200 x 50 + 10 + 50)
        assert_eq!(size.width, 310.0);
        assert_eq!(size.height, 110.0);
    }
}