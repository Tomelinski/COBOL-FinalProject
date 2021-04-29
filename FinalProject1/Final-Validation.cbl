       identification division.
       program-id. Final-DataValidation.
       author. name. Tom Zielinski. Calvin May.
       date-written. date. 03/30/2021
      *Last-Editted:       04/15/2021
      * Description: This Program takes data records from a project6.dat
      *             file and validates all the data. Invalid data is
      *             Processed and recorded into an error report, the raw
      *             data is also sent to an invalid record output file.
      *             whereas valid data is sent to a valid record output
      *             file.

      *  Tom & Calvin worked on this file.

       environment division.

       input-output section.
       file-control.

           select data-file
               assign to "../../../../data/project6.dat"
                  organization is line sequential.

           select valid-file
               assign to "../../../../data/Final-Valid.dat"
               organization is line sequential.
       
           select invalid-file
               assign to "../../../../data/Final-Invalid.dat"
               organization is line sequential.

           select error-file
               assign to "../../../../data/Final-Error-Report.out"
               organization is line sequential.               

       configuration section.

       data division.
       file section.
       fd data-file
           data record is item-rec
           record contains 36 characters.
      *
       
       01 item-rec.
         05 it-code                    pic x.
           88 it-code-valid                value 'S' 'R' 'L'.
         05 it-transaction             pic 9(5)v99.
         05 it-payment-type            pic xx.
           88 it-payment-valid             value 'CA' 'CR' 'DB'.
         05 it-store-num               pic 99.
           88 it-store-num-valid           value 1 thru 5, 12.
         05 it-invoice.
           10 it-invoice-prefix-one    pic x.
             88 it-invoice-prefix1-valid   value 'A' 'B' 'C' 'D' 'E'.
           10 it-invoice-prefix-two    pic x.
             88 it-invoice-prefix2-valid   value 'A' 'B' 'C' 'D' 'E'.
           10 it-dash                  pic x.
             88 it-invoice-dash-valid      value '-'.
           10 it-invoice-num           pic 9(6).
             88 it-invoice-num-valid       value 100000 thru 900000.
           10 it-ven-num-rest          pic 9(5).
               88 it-ven-num-valid         value 1, 2, 3.
         05 it-sku-code                pic x(15).         

      *
       fd valid-file
           data record is valid-line
           record contains 36 characters.

       01 valid-line                   pic x(36).

       fd invalid-file
           data record is invalid-line
           record contains 36 characters.

       01 invalid-line                 pic x(36).

       fd error-file
           data record is error-line
           record contains 69 characters.

       01 error-line                   pic x(69).

       working-storage section.

       01 ws-heading1-name-line.
         05 filler                     pic x(25)
               value "Tom Zielinski, Calvin May".
         05 filler                     pic x(10)   value space.
         05 filler                     pic x(34)
               value "Final: Program 1 - Data Validation".

       01 ws-heading2-title.
         05 filler                     pic x(28)   value spaces.
         05 filler                     pic x(12)   value "ERROR REPORT".

       01 ws-heading3-headings.
         05 filler                     pic x(6)    value "RECORD".
         05 filler                     pic x(52)   value spaces.

       01 ws-heading4-headings.
         05 filler                     pic x(6)    value "NUMBER".
         05 filler                     pic x(2)    value space.
         05 filler                     pic x(37) 
               value "---------------RAW DATA--------------".
         05 filler                     pic x(2)    value space.
         05 filler                     pic x(22)
               value "----ERROR MESSAGES----".

       01 ws-detail-line.
         05 filler                     pic x(2)    value space.
         05 ws-rec-num                 pic zz9.
         05 filler                     pic x(4)    value space.
         05 ws-item                    pic x(36).
         05 filler                     pic x(2)    value space.
         05 ws-error                   pic x(7).

       01 ws-error-display.
         05 filler                     pic x(9)    value spaces.
         05 ws-error-underline.
           10 ws-underline-code        pic x.
           10 ws-underline-trans       pic x(7).
           10 ws-underline-payment     pic xx.
           10 ws-underline-store       pic xx.
           10 ws-underline-invoice.
               15 ws-underline-prefix  pic xx.
               15 ws-underline-dash    pic x.
               15 ws-underline-num     pic x(6).
           10 ws-underline-SKU         pic x(15).
         05 filler                     pic x(2)    value spaces.
         05 ws-error-info-display      pic x(24).


       

       01 ws-error-table.
         05 ws-error-info              pic x(24)   occurs 8 times.

       01 ws-error-summary.
         05 filler                     pic x(26)
               value "-------ERROR SUMMARY------".

       01 ws-line1-total.
         05 filler                     pic x(23)
               value "TOTAL RECORDS =".
         05 ws-total-records-edit      pic zz9.

       01 ws-line2-total.
         05 filler                     pic x(23)
               value "TOTAL VALID RECORDS =".
         05 ws-total-valid-edit        pic zz9.

       01 ws-line3-total.
         05 filler                     pic x(23)
               value "TOTAL INVALID RECORDS =".
         05 ws-total-invalid-edit      pic zz9.

       01 ws-totals.
         05 ws-total-records           pic 999     value 0.
         05 ws-total-valid             pic 999     value 0.
         05 ws-total-invalid           pic 999     value 0.


       01 ws-flags.
         05 ws-eof-flag                pic x       value 'n'.
         05 ws-valid-data              pic x       value 'y'.
         05 ws-new-page                pic x       value 'y'.

       01 ws-counters.
         05 ws-page-count              pic 9       value 1.
         05 ws-line-count              pic S99     value 0.

       01 ws-index                     pic 9       value 0.

       77 ws-lines-per-page            pic 99      value 15.
       77 ws-first-page                pic 9       value 1.
       77 ws-zero                      pic 9       value 0.
       77 ws-one                       pic 9       value 1.
       77 ws-two                       pic 9       value 2.
       77 ws-three                     pic 9       value 3.
       77 ws-four                      pic 9       value 4.
       77 ws-five                      pic 9       value 5.
       77 ws-six                       pic 9       value 6.
       77 ws-seven                     pic 9       value 7.
       77 ws-eight                     pic 9       value 7.
       77 ws-y                         pic x       value "y".
       77 ws-n                         pic x       value "n".
       77 ws-valid                     pic x(5)    value "VALID".
       77 ws-invalid                   pic x(7)    value "INVALID".
       77 ws-underline                 pic x(15)
               value "---------------"                                  .

       77 ws-err-trns-co               pic x(22)
               value "INVALID TRANS CODE".
       77 ws-err-trans                 pic x(22)
               value "INVALID TRANS PRICE".
       77 ws-err-payment               pic x(22)
               value "INVALID PAYMENT TYPE".
       77 ws-err-store                 pic x(22)
               value "INVALID STORE NUMBER".
       77 ws-err-invoice-pre           pic x(22)
               value "INVALID INVOICE PREFIX".
       77 ws-err-invoice-num           pic x(22)
               value "INVALID INVOICE NUMBER".
       77 ws-err-invoice-oor           pic x(22)
               value "INVOICE OUT OF RANGE".
       77 ws-err-invoice-dash          pic x(22)
               value "INVALID INVOICE FORMAT".
       77 ws-err-sku                   pic x(22)
               value "INVALID SKU CODE".

       procedure division.
       000-main.
      *Open read/write files
           open input data-file.
           open output valid-file, invalid-file, error-file.

      *set end of file variable
           perform 010-read-file.

      *read file until end of file is reached
      *also, call calculate and display functions
           perform 100-process-pages
               until ws-eof-flag equals ws-y.

           perform 500-report-footer.

           close data-file, valid-file, invalid-file, error-file.


           goback.

       010-read-file.
           read data-file
               at end
                   move ws-y to ws-eof-flag.

       100-process-pages.
      *print lines per page 
           perform 200-process-lines
             varying ws-line-count from ws-zero by ws-one
               until ws-line-count equals ws-lines-per-page
                   or ws-eof-flag equals ws-y.

      *increase page number by 1 after page has been read
           add ws-one              to ws-page-count.
           move ws-y               to ws-new-page.


       200-process-lines.
      *reset variables
           move spaces             to ws-error-table.
           move ws-valid           to ws-error.

           add ws-one              to ws-total-records.
           move ws-total-records   to ws-rec-num.
           move ws-y               to ws-valid-data.

      *check 88 variable and validate
           if (not it-code-valid)              then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-code
               move ws-err-trns-co to ws-error-info(ws-one)
           end-if.

      *check 88  variableand validate
           if (it-transaction is not numeric)  then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-trans
               move ws-err-trans   to ws-error-info(ws-two)
           
           end-if.

      *check 88 variable and validate if price is numeric
           if (not it-payment-valid)           then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-payment
               move ws-err-payment to ws-error-info(ws-three)
           end-if.

      *check 88 variable and validate
           if  (not it-store-num-valid)        then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-store
               move ws-err-store   to ws-error-info(ws-four)
           end-if.
           
      *check 88 variable and validate
           if (not it-invoice-prefix1-valid    or
               not it-invoice-prefix2-valid    or
               it-invoice-prefix-one =
               it-invoice-prefix-two)          then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-prefix
               move ws-err-invoice-pre
                                   to ws-error-info(ws-five)
           end-if.

           if (not it-invoice-dash-valid)      then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-dash
               move ws-err-invoice-dash
                                   to ws-error-info(ws-six)
           end-if.

           if (it-invoice-num is not numeric)  then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-num
               move ws-err-invoice-num
                                   to ws-error-info(ws-seven)
           else
           if (not it-invoice-num-valid)       then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-num
               move ws-err-invoice-oor
                                   to ws-error-info(ws-seven)
           end-if.
      *check 88 variable and validate
           if (it-sku-code) = spaces           then
               move ws-n           to ws-valid-data
               move ws-underline   to ws-underline-sku
               move ws-err-sku     to ws-error-info(ws-eight)
           end-if.

      *check if row is not valid
           if ws-valid-data = ws-n             then
      *print headings, if record is invalid and on first and 15th record
               perform 300-print-headings
               move ws-n           to ws-new-page

               perform 310-invalid-record
           else
               perform 320-valid-record
           end-if.

      *check if end of file 
           perform 010-read-file.

       300-print-headings.
      * check if the page requires a page header & only print after an 
      *invlaid record
           if ws-new-page = ws-y               then

      *display name on first page
           if (ws-page-count = ws-first-page)  then
               write error-line from ws-heading1-name-line
                 after advancing ws-one line
               write error-line from ws-heading2-title
                 after advancing ws-two line
           else
      *display extra space if not first page
               write error-line from spaces
               write error-line from ws-heading2-title
                 after advancing page
           end-if

      *print page header and title
           write error-line    from ws-heading3-headings
             after advancing ws-two line
           write error-line    from ws-heading4-headings
             after advancing ws-one line
           write error-line    from spaces
             after advancing ws-one line
           end-if.

       310-invalid-record.

      * Write to Invalid Data File
       write invalid-line      from item-rec.
       
      *add to summary variables and totals
           add ws-one      to ws-total-invalid.
           move ws-y       to ws-valid-data.
           move item-rec   to ws-item.
           move ws-invalid to ws-error.

           write error-line    from ws-detail-line
             after advancing ws-one line.
      *loop through error array - display none blanks
           perform 400-display-error
             varying ws-index  from ws-one by ws-one
             until ws-index > ws-eight.

       320-valid-record.
           write valid-line    from item-rec.

           subtract ws-one     from ws-line-count.
           add ws-one      to ws-total-valid.

      *    perform
       400-display-error.
      *move the array at index value into display variables
           move ws-error-info(ws-index) to ws-error-info-display.

           if ws-error-info-display <> space then
               write error-line from ws-error-display
               move spaces              to ws-error-underline
           end-if.


       500-report-footer.
           move ws-total-records        to ws-total-records-edit.

      *Move summary vaiables into display variables
           move ws-total-valid          to ws-total-valid-edit.
           move ws-total-invalid        to ws-total-invalid-edit.

      * write report line in file
           write error-line             from ws-error-summary
             after advancing ws-two line.

           write error-line             from ws-line1-total
             after advancing ws-two line.
           write error-line             from ws-line2-total
             after advancing ws-one line.
           write error-line             from ws-line3-total
             after advancing ws-one line.

       end program Final-DataValidation.