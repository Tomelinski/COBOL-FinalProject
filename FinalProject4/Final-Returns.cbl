       identification division.
       program-id. Final-ReturnProccess.
       author. name. Tom Zielinski. Calvin May.
       date-written. date. 03/30/2021
      *Last-Edited:        04/15/2021
      *Description: This Report Outputs the Details for Returned 
      *            Records, and provides a quick summary per store.

      * Tom & Calvin worked on this Program.

       environment division.

       input-output section.
       file-control.

           select data-file
               assign to "../../../../data/Final-return-records.dat"
               organization is line sequential.

           select report-file
               assign to "../../../../data/Final-return-summary.out"
               organization is line sequential.     

       configuration section.

       data division.
       file section.
       fd data-file
           data record is item-rec
           record contains 36 characters.
      *
       
       01 item-rec.
         05 it-code                 pic x.
         05 it-transaction          pic 9(5)v99.
         05 it-payment-type         pic xx.
           88 it-ca                     value 'CA'.
           88 it-cr                     value 'CR'.
           88 it-db                     value 'DB'.
         05 it-store-num            pic 99.
           88 it-store-1                value 1.
           88 it-store-2                value 2.
           88 it-store-3                value 3.
           88 it-store-4                value 4.
           88 it-store-5                value 5.
           88 it-store-12               value 12.
         05 it-invoice              pic x(9).
         05 it-sku-code             pic x(15).         


       fd report-file
           data record is report-line
           record contains 79 characters.

       01 report-line               pic x(79).

       working-storage section.

       01 ws-heading1-name-line.
         05 filler                  pic x(25)
               value "Tom Zielinski, Calvin May".
         05 filler                  pic x(5)
               value space.
         05 filler                  pic x(37)
               value "Final: Program 4 - Return Proccessing".

       01 ws-heading2-title.
         05 filler                  pic x(23)   value spaces.
         05 filler                  pic x(23)
               value "RETURNED SUMMARY REPORT".

       01 ws-heading3-headings. 
         05 filler                  pic x(5)    value "TRANS".
         05 filler                  pic x(10)   value spaces.
         05 filler                  pic x(7)    value "PAYMENT".
         05 filler                  pic x(2)    value spaces.
         05 filler                  pic x(5)    value "STORE".
         05 filler                  pic x(2)    value spaces.
         05 filler                  pic x(7)    value "INVOICE".
         05 filler                  pic x(8)    value spaces.
         05 filler                  pic x(3)    value "SKU".
         05 filler                  pic x(12)   value spaces.
         05 filler                  pic x(3)    value "TAX".
                                    
       01 ws-heading4-headings.     
         05 filler                  pic x(4)    value "CODE".
         05 filler                  pic x(4)    value spaces.
         05 filler                  pic x(5)    value "PRICE".
         05 filler                  pic x(3)    value spaces.
         05 filler                  pic x(4)    value "TYPE".
         05 filler                  pic x(6)    value spaces.
         05 filler                  pic x(1)    value "#".
         05 filler                  pic x(7)    value spaces.
         05 filler                  pic x(1)    value "#".
         05 filler                  pic x(11)   value spaces.
         05 filler                  pic x(4)    value "CODE".
         05 filler                  pic x(11)   value spaces.
         05 filler                  pic x(4)    value "OWED".
                                    
       01 ws-detail-line.           
         05 filler                  pic x(2)    value spaces.
         05 ws-code                 pic x.
         05 filler                  pic x(1)    value spaces.
         05 ws-transaction          pic $$$,$$9.99.
         05 filler                  pic x(2)    value spaces.
         05 ws-payment-type         pic x(6).
         05 filler                  pic x(4)    value spaces.
         05 ws-store-num-edit       pic z9.
         05 filler                  pic x(3)    value spaces.
         05 ws-invoice              pic x(9).
         05 filler                  pic x(2)    value spaces.
         05 ws-sku-code             pic x(15).
         05 filler                  pic x(1)    value spaces.
         05 ws-tax-edit             pic $$,$$9.99.
                                    
       01 ws-math.                  
         05 ws-price                pic 9(5)v99.
         05 ws-tax                  pic 9(5)v99.

       01 ws-summary-header.
         05 filler pic x(23) value spaces.
         05 filler pic x(22) value "Return Report Summary".

       01 ws-summary-header2.
         05 filler pic x(4) value spaces.
         05 filler pic x(5) value "STORE".
         05 filler pic x(5) value spaces.
         05 filler pic x(7) value "Total R".

       01 ws-summary-header3.
         05 filler pic x(6) value spaces.
         05 filler pic x(1) value "#".
         05 filler pic x(7) value spaces.
         05 filler pic x(7) value "Records".

       01 ws-display-total-r.
         05 filler                  pic x(4)    value spaces.
         05 filler                  pic x(6)    value "STORE ".
         05 ws-store-num-r-edit     pic z9.
         05 filler                  pic x(7)    value ": ".
         05 ws-r-count-edit         pic z9.

       01 ws-summary-line3.
         05 filler                  pic x(4)   value spaces.
         05 filler                  pic x(17)
               value "TOTAL R RECORDS: ".
         05 ws-total-records-edit   pic zz9.

       01 ws-summary-line4.
         05 filler                  pic x(11)  value spaces.
         05 filler                  pic x(10)  value "Tax Owed: ".
         05 ws-total-tax-edit       pic $$9.99.


       01 ws-totals.
         05 ws-total-records        pic 999     value 0.
         05 ws-total-tax            pic 9(4)v99 value 0.
         
       01 ws-stores.
         05 ws-store occurs 6 times.
           10 ws-store-num          pic 99
                   value 01, 02, 03, 04, 05, 12.
           10 ws-r-count            pic 99      value 0.
                                    
       01 ws-flags.                 
         05 ws-eof-flag             pic x       value 'n'.
         05 ws-new-page             pic x       value 'y'.
                                    
       01 ws-counters.              
         05 ws-page-count           pic 9       value 1.
         05 ws-line-count           pic S99     value 0.
         05 ws-index                pic 99      value 1.
                                    
       77 ws-lines-per-page         pic 99      value 20.
       77 ws-first-page             pic 9       value 1.
       77 ws-zero                   pic 9       value 0.
       77 ws-one                    pic 9       value 1.
       77 ws-two                    pic 9       value 2.
       77 ws-three                  pic 9       value 3.
       77 ws-four                   pic 9       value 4.
       77 ws-five                   pic 9       value 5.
       77 ws-six                    pic 9       value 6.
       77 ws-cash                   pic x(6)    value "CASH".
       77 ws-credit                 pic x(6)    value "CREDIT".
       77 ws-debit                  pic x(6)    value "DEBIT".
       77 ws-the-tax                pic 9v99    value 0.13.
       77 ws-y                      pic x       value "y".

       procedure division.
       000-main.
      *Open read/write files
           open input data-file.
           open output report-file.

      *set end of file variable
           perform 010-read-file.

      *read file until end of file is reached
      *also, call calculate and display functions
           perform 100-process-pages
               until ws-eof-flag equals ws-y.
     
      *    perform 330-calculate-percents.
      *
           perform 400-report-footer.

           close data-file, report-file.


           goback.

       010-read-file.
           read data-file
               at end
                   move ws-y to ws-eof-flag.

       100-process-pages.

           perform 200-print-headings.

      *print lines per page 
           perform 300-process-lines
             varying ws-line-count from ws-zero by ws-one
               until ws-line-count equals ws-lines-per-page
                   or ws-eof-flag  equals ws-y.

      *increase page number by 1 after page has been read
           add ws-one  to ws-page-count.
           move ws-y   to ws-new-page.

       200-print-headings.
      * check if the page requires a page header & only print after an
      *invlaid record
           if ws-new-page = ws-y                   then

      *display name on first page
               if (ws-page-count = ws-first-page)  then
                   write report-line from ws-heading1-name-line
                     after advancing ws-one line
                   write report-line from ws-heading2-title
                     after advancing ws-two line
               else
      *display extra space if not first page
                   write report-line from spaces
                   write report-line from ws-heading2-title
                     after advancing page
               end-if

      *print page header and title
               write report-line     from ws-heading3-headings
                 after advancing ws-two line
               write report-line     from ws-heading4-headings
                 after advancing ws-one line
               write report-line     from spaces
                 after advancing ws-one line
           end-if.




       300-process-lines.

           move it-code            to ws-code.
           move it-transaction     to ws-transaction.
           move it-transaction     to ws-price.

           if it-ca    then
               move ws-cash        to ws-payment-type
           else
           if it-cr    then
               move ws-credit      to ws-payment-type
           else
           if it-db    then
               move ws-debit       to ws-payment-type
           end-if
           end-if
           end-if.

           move it-store-num       to ws-store-num-edit.
           move it-invoice         to ws-invoice.
           move it-sku-code        to ws-sku-code.

           perform 310-calc-tax.

           add ws-one              to ws-total-records.


           if (it-store-1) then
               add ws-one to ws-r-count(ws-one)  
           else
           if (it-store-2)  then
               add ws-one to ws-r-count(ws-two)  
           else
           if (it-store-3) then
               add ws-one to ws-r-count(ws-three)  
           else
           if (it-store-4)  then
               add ws-one to ws-r-count(ws-four)  
           else
           if (it-store-5)  then
               add ws-one to ws-r-count(ws-five)  
           else
           if (it-store-12)  then
               add ws-one to ws-r-count(ws-six)  
           else
           end-if
           end-if
           end-if.


      * Write to Detail Line
           write report-line       from ws-detail-line.


      *check if end of file 
           perform 010-read-file.


       310-calc-tax.

           compute ws-tax rounded = (ws-price * ws-the-tax).

           move ws-tax             to ws-tax-edit.
           add ws-tax              to ws-total-tax.


       400-report-footer.
      *    Move Totals...
           move ws-total-records   to ws-total-records-edit.
           move ws-total-tax       to ws-total-tax-edit.


           write report-line  from ws-summary-header
             after advancing ws-one line.
           write report-line from ws-summary-header2
             after advancing ws-two lines.
           write report-line from ws-summary-header3
             after advancing ws-one line.
           write report-line from spaces.

      * Display data for R records 
           perform 600-display-store-r
             varying ws-index from ws-one by ws-one
             until ws-index > ws-six.

      * Display Data for Total Records
           write report-line from ws-summary-line3
             after advancing 1 line.
           write report-line from ws-summary-line4.


     

       600-display-store-r.
           move ws-store-num(ws-index)
             to ws-store-num-r-edit.
           move ws-r-count(ws-index)
             to ws-r-count-edit.
           write report-line from ws-display-total-r.

       end program Final-ReturnProccess.