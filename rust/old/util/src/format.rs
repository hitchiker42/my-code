
pub fn format_number_english<T: num::Integer>(n: T) -> String {
    let ones_list = ["","one", "two", "three", "four", "five", "six", "seven", "eight",
                     "nine", "ten", "eleven", "twelve", "thirteen", "fourteen",
                     "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"];
    let test_list = ["","","twenty", "thirty", "forty", "fifty",
                     "sixty", "seventy", "eighty", "ninety"];
    //Ideally we'd allocate this with capacity log_10(n), but there's no generic
    //log function on Integers
    let digits: Vec<&str> = Vec::new();
    fn num_to_cardinal999(n: T){
        let hundreds = n / 100;
        let tens_and_ones = n % 100;
        let tens = tens_and_ones / 10;
        let ones = tens_and_ones % 10;
        if hundreds > 0 {
            digits.push(ones_list[hundreds]);            
            digits.push(if tens_and_ones > 0 {" hundred "} else {" hundred"});
            
        
}
        
    
