J Ь    Ь                                                         '                BILLNAME                                  !   '                BILLADR1                                  A   '                BILLADR2                                  a                  BILLCITY                                  s                  BILLSTAT                                  u   ' 
              BILLZIP                                                     BILLPHON                                  Ї  / N                SHIPNAME                                  Г  / N                SHIPADR1                                  ═  / N                SHIPADR2                                  ь  / N                SHIPCITY                                                   INVOICEN                                                   INVDATE               ­?     бя@            *               CUSPO                                     , , 9               TERMS                                     : ; O               SALESMAN                                  O  . (              DESCR1                                    w 0 3               UM1                                       { 5 <               QUANT1                Я└    └ ▀@          } > E               UNITP1                 Р     №$         Ё G O               AMOUNT1        Р     № Р     №$        Ї  . (              DESCR2                                    х 0 3               UM2                                       ╣ 5 <               QUANT2                Я└    └ ▀@          ╗ > E               UNITP2                 Р     №$         ├ G O               AMOUNT2        Р     № Р     №$        ╦  . (              DESCR3                                    з 0 3               UM3                                       э 5 <               QUANT3                Я└    └ ▀@          щ > E               UNITP3                 Р     №$          G O               AMOUNT3        Р     № Р     №$        	  . (              DESCR4                                    1 0 3               UM4                                       5 5 <               QUANT4                Я└    └ ▀@          7 > E               UNITP4                 Р     №$         ? G O               AMOUNT4        Р     № Р     №$        G  . (              DESCR5                                    o 0 3               UM5                                       s 5 <               QUANT5                Я└    └ ▀@          u > E               UNITP5                 Р     №$         } G O               AMOUNT5        Р     № Р     №$        Ё  . (              DESCR6                                    Г 0 3               UM6                                       ▒ 5 <               QUANT6                Я└    └ ▀@          │ > E               UNITP6                 Р     №$         ╗ G O               AMOUNT6        Р     № Р     №$        ├  . (              DESCR7                                    в 0 3               UM7                                       № 5 <               QUANT7                Я└    └ ▀@          ы > E               UNITP7                 Р     №$         щ G O               AMOUNT7        Р     № Р     №$          . (              DESCR8                                    ) 0 3               UM8                                       - 5 <               QUANT8                Я└    └ ▀@          / > E               UNITP8                 Р     №$         7 G O               AMOUNT8        Р     № Р     №$        ?  . (              DESCR9                                    g 0 3               UM9                                       k 5 <               QUANT9                Я└    └ ▀@          m > E               UNITP9                 Р     №$         u G O               AMOUNT9        Р     № Р     №$        }  . (              DESCR10                                   Ц 0 3               UM10                                      Е 5 <               QUANT10               Я└    └ ▀@          Ф > E               UNITP10                Р     №$         │ G O               AMOUNT10       Р     № Р     №$        ╗ E O               SUBTOTAL       Р     № Р     №$        ├                 DISCRATE                      Y@          ┼ E O               DISCOUNT       Р     № Р     №$         ═ E O               SALESTAX               Р     №$        Н                 TAXRATE                       Y@          О ) 4               EXEMPT                                    с E O               TOTALDUE       Р     № Р     №$        в"  N               NOTES         )                           C



   


BILLNAME
BILLADR1
BILLADR2
BILLCITY + ", " + BILLSTAT + " " + BILLZIP

@TODAY







QUANT1*UNITP1




QUANT2*UNITP2




QUANT3*UNITP3




QUANT4*UNITP4




QUANT5*UNITP5




QUANT6*UNITP6




QUANT7*UNITP7




QUANT8*UNITP8




QUANT9*UNITP9




QUANT10*UNITP10
AMOUNT1+AMOUNT2+AMOUNT3+AMOUNT4+AMOUNT5+AMOUNT6+AMOUNT7+AMOUNT8+AMOUNT9+AMOUNT10

SUBTOTAL*DISCRATE/100*-1
TAXRATE / 100 * SUBTOTAL * ABS(EXEMPT = "            " AND BILLSTAT = "CT")
   
   
SUBTOTAL+DISCOUNT+SALESTAX

HBilling company name.  The first letter of each word will automatically be capitalized as you type.  After entry this field will be copied to the shipping address.
Billing address line #1.  The first letter of each word will automatically be capitalized as you type.  After entry this field will be copied to the shipping address.
Billing address line #2.  The first letter of each word will automatically be capitalized as you type.  After entry this field will be copied to the shipping address.
Billing city name.  The first letter of each word will automatically be capitalized as you type.  After entry this field will be copied to the shipping address.
Billing state.  Letters are automatically capitalized.  If state is not [CT], no sales tax will be charged.  After entry this field will be copied to the shipping address.
Billing zip code.  After entry this field will be copied to the shipping address. 
Phone number of billing department.
Shipping address name.
Shipping address line #1
Shipping address line #2
Shipping address - city, state and zip code. 
Number for this invoice
Date of invoice.  Use todays date if this is a new invoice 
Customer's purchase order number for their reference.
Payment terms.  Press [Enter] to select from a list of choices. 
Salesman's name.  Press [Enter] to select from a list of choices 
Description of the item.  Press Enter to select this from a list of stock items. 
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items. 
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items. 
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items. 
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items. 
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items. 
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF" 
Quantity or number of items. 
Unit price or cost per item. 

Description of the item.  Press Enter to select this from a list of stock items.
Units of measure ie. "EA", "LB", "FT", "IN", "SQF"
Quantity or number of items. 
Unit price or cost per item. 


Discount rate to apply to the total cost of goods as a percentage. 
Discount in dollars.
The sales tax is computed from the SUB TOTAL and SALES TAX RATE fields if the customer is based in Connecticut and is not exempt (Tax ID number is required).
Our state's sales tax rate as a percentage. 
Enter the buyer's state tax identification number if applicable. 

Enter any sales notes or special shipping instructions. 
