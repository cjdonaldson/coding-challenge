/** x1b[ground:colormode:...:0m
* https://en.wikipedia.org/wiki/ANSI_escape_code
* https://i.sstatic.net/9UVnC.png
* 0  colormode 3, 4 bit pallet
* 38 foreground
* 48 background
* 2 colormode 24 bit rgb sequence
* 5 colormode  8 bit pallet
*/
pub const RESET: &str = "\x1b[0m";
pub const ORANGE: &str = "\x1b[38;2;255;165;0m";
pub const RED: &str = "\x1b[0;31m";
pub const GREEN: &str = "\x1b[0;32m";
pub const BLUE: &str = "\x1b[0;34m";
