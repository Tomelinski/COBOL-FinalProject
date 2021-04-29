       identification division.
       program-id. SAndLProcessing.
       author. name. Calvin May.
       date-written. date. 03/30/2021
      *Last-Edited:  date. 04/07/2021
      *Description: This Program processes all validated S and L
      *            Records and provides a detailed report summary.
      * *************************************************************|
       environment division.
      * **
       input-output section.
       file-control.
      * Input/Data File
           select data-file
               assign to "../../../../data/Final-sl-records.dat"
               organization is line sequential.

      * Output/Report File 
           select report-file
               assign to "../../../../data/Final-sl-summary.out"
               organization is line sequential.

      * ** 
       configuration section.

      * *************************************************************| 
       data division.
      * ** 
       file section.
      * Input/Data File Definition, each record is an Item Record,
      *-36 Characters long.
       fd data-file
           data record is item-rec
           record contains 36 characters.
      *
       01 item-rec.
         05 it-code                pic x.
           88 it-code-l                value 'L'.
         05 it-transaction         pic 9(5)v99.
         05 it-payment-type        pic xx.
           88 it-ca                    value 'CA'.
           88 it-cr                    value 'CR'.
           88 it-db                    value 'DB'.
         05 it-store-num           pic 99.
         05 it-invoice             pic x(9).
         05 it-sku-code            pic x(15).

      * Output/Report File Definition, each Report Record is 67
      *-Characters long.
       fd report-file
           data record is report-line
           record contains 67 characters.
      *
       01 report-line              pic x(67).

      * ** 
       working-storage section.
      *
       01 ws-heading1-name-line.
         05 filler                 pic x(25)
               value "Tom Zielinski, Calvin May".
         05 filler                 pic x(6)    value space.
         05 filler                 pic x(36)
               value "Final: Program 3 - S & L Proccessing".
      *
       01 ws-heading2-title.
         05 filler                 pic x(27)   value spaces.
         05 filler                 pic x(12)   value "S & L Report".
         05 filler                 pic x(21)   value spaces.
         05 filler                 pic x(6)    value "Page: ".
         05 ws-page-number         pic x(1).
      *
       01 ws-heading3-headings.
         05 filler                 pic x(5)    value "TRANS".
         05 filler                 pic x(11)   value spaces.
         05 filler                 pic x(7)    value "PAYMENT".
         05 filler                 pic x(2)    value spaces.
         05 filler                 pic x(5)    value "STORE".
         05 filler                 pic x(3)    value spaces.
         05 filler                 pic x(7)    value "INVOICE".
         05 filler                 pic x(7)    value spaces.
         05 filler                 pic x(3)    value "SKU".
         05 filler                 pic x(13)   value spaces.
         05 filler                 pic x(3)    value "TAX".
      *
       01 ws-heading4-headings.
         05 filler                 pic x(4)    value "CODE".
         05 filler                 pic x(5)    value spaces.
         05 filler                 pic x(5)    value "PRICE".
         05 filler                 pic x(3)    value spaces.
         05 filler                 pic x(4)    value "TYPE".
         05 filler                 pic x(6)    value spaces.
         05 filler                 pic x(1)    value "#".
         05 filler                 pic x(8)    value spaces.
         05 filler                 pic x(1)    value "#".
         05 filler                 pic x(10)   value spaces.
         05 filler                 pic x(4)    value "CODE".
         05 filler                 pic x(12)   value spaces.
         05 filler                 pic x(4)    value "OWED".
      *
       01 ws-detail-line.
         05 filler                 pic x(2)    value spaces.
         05 ws-code                pic x.
         05 filler                 pic x(1)    value spaces.
         05 ws-transaction         pic $$$,$$9.99.
         05 filler                 pic x(2)    value spaces.
         05 ws-payment-type        pic x(6).
         05 filler                 pic x(4)    value spaces.
         05 ws-store-num-edit      pic z9.
         05 filler                 pic x(3)    value spaces.
         05 ws-invoice             pic x(9).
         05 filler                 pic x(2)    value spaces.
         05 ws-sku-code            pic x(15).
         05 filler                 pic x(1)    value spaces.
         05 ws-tax-edit            pic $$,$$9.99.
      *
       01 ws-summary-header.
         05 filler                 pic x(23)   value spaces.
         05 filler                 pic x(20)
               value "S & L Report Summary".
      *
       01 ws-summary-line1.
         05 filler                 pic x(4)    value spaces.
         05 filler                 pic x(19)
               value "Total S&L Records: ".
         05 ws-total-recs          pic zz9     value 100.
      *
       01 ws-summary-line2.
         05 filler                 pic x(6)    value spaces.
         05 filler                 pic x(17)
               value "Total S Records: ".
         05 ws-total-s-recs        pic zz9     value 100.
      *
       01 ws-summary-line3.
         05 filler                 pic x(6)    value spaces.
         05 filler                 pic x(17)
               value "Total L Records: ".
         05 ws-total-l-recs        pic zz9.
      *
       01 ws-summary-line4.
         05 filler                 pic x(4)    value spaces.
         05 filler                 pic x(24)
               value "Payment Type Occurences:".
         05 filler                 pic x(5)    value spaces.
         05 filler                 pic x(4)    value "Cash".
         05 filler                 pic x(5)    value spaces.
         05 filler                 pic x(6)    value "Credit".
         05 filler                 pic x(5)    value spaces.
         05 filler                 pic x(5)    value "Debit".
      *
       01 ws-summary-line5.
         05 filler                 pic x(13)   value spaces.
         05 filler                 pic x(15)
               value "(Number of)  #:".
         05 filler                 pic x(6)    value spaces.
         05 ws-total-ca            pic zz9.
         05 filler                 pic x(8)    value spaces.
         05 ws-total-cr            pic zz9.
         05 filler                 pic x(7)    value spaces.
         05 ws-total-db            pic zz9.
      *
       01 ws-summary-line6.
         05 filler                 pic x(12)   value spaces.
         05 filler                 pic x(16)
               value "(Percentage)  %:".
         05 filler                 pic x(4)    value spaces.
         05 ws-perc-ca             pic z9.9    value 12.5.
         05 filler                 pic x       value "%".
         05 filler                 pic x(6)    value spaces.
         05 ws-perc-cr             pic z9.9    value 12.5.
         05 filler                 pic x       value "%".
         05 filler                 pic x(5)    value spaces.
         05 ws-perc-db             pic z9.9    value 12.5.
         05 filler                 pic x       value "%".
      *
       01 ws-summary-line7.
         05 filler                 pic x(4)    value spaces.
         05 filler                 pic x(16)
               value "Total Tax Owed: ".
         05 ws-total-tax           pic $$$,$$9.99.
      *
       01 ws-summary-line8.
         05 filler                 pic x(4)    value spaces.
         05 filler                 pic x(14)   value "Store Number: ".
         05 ws-store-highest       pic z9.
         05 filler                 pic x(47)
               value ", had the Highest S&L Total Transaction Amount.".
      *
       01 ws-summary-line9.
         05 filler                 pic x(4)    value spaces.
         05 filler                 pic x(14)   value "Store Number: ".
         05 ws-store-lowest        pic z9.
         05 filler                 pic x(47)
               value ", had the Highest S&L Total Transaction Amount.".
      *
       01 ws-stores.
         05 ws-store occurs 6 times.
           10 ws-store-num         pic 99      value 01 02 03 04 05 12.
           10 ws-store-trans       pic 9(6)v99.
         05 ws-store-count         pic 9       value 6.
      *
       01 ws-flags.
         05 ws-eof-flag            pic x       value 'n'.
         05 ws-new-page            pic x       value 'y'.
      *
       01 ws-counters.
         05 ws-page-count          pic 9       value 1.
         05 ws-line-count          pic 999     value 0.
         05 ws-rec-count           pic 999     value 0.
         05 ws-s-count             pic 999     value 0.
         05 ws-l-count             pic 999     value 0.
         05 ws-type-ca-count       pic 999     value 0.
         05 ws-type-cr-count       pic 999     value 0.
         05 ws-type-db-count       pic 999     value 0.
      *
       01 ws-totals.
         05 ws-tax-running-total   pic 9(6)v99 value 0.
         05 ws-ca-total-perc       pic 99v99   value 0.
         05 ws-cr-total-perc       pic 99v99   value 0.
         05 ws-db-total-perc       pic 99v99   value 0.
      *
       77 ws-lines-per-page        pic 99      value 15.
       77 ws-first-page            pic 9       value 1.
       77 ws-zero                  pic 9       value 0.
       77 ws-one                   pic 9       value 1.
       77 ws-perc-modifier         pic 999     value 100.
       77 ws-cash                  pic x(6)    value "CASH".
       77 ws-credit                pic x(6)    value "CREDIT".
       77 ws-debit                 pic x(6)    value "DEBIT".
       77 ws-the-tax               pic 9v99    value 0.13.
       77 ws-y                     pic x       value "y".
       77 ws-blank-line            pic x(67)   value spaces.
       77 ws-sub                   pic 9       value 1.
       77 ws-current-highest       pic 9(6)v99 value 0.
       77 ws-current-lowest        pic 9(6)v99 value 0.
       77 ws-tax                   pic 9(5)v99.

      * *************************************************************| 
       procedure division.
       000-main.
      * Open Read/Write Files
           open input data-file.
           open output report-file.

      * Read Initial Record
           perform 010-read-file.

      * Read file until end of file is reached
           perform 100-process-pages
             until ws-eof-flag equals ws-y.

      * Write the Report SUmmary
           perform 400-report-summary.

      * Close the Read/Write Files
           close data-file, report-file.

           goback.

       010-read-file.
           read data-file
               at end
                   move ws-y to ws-eof-flag.

       100-process-pages.

      * Write the Headings
           perform 200-print-headings.

      * Process Lines, untill Record Max per Page OR End-Of-File
           perform 300-process-lines
             varying ws-line-count     from ws-zero    by ws-one
               until ws-line-count equals ws-lines-per-page
               or ws-eof-flag      equals ws-y.

      * Increment page number by 1 after page has been read
           add ws-one                  to ws-page-count.
           move ws-y                   to ws-new-page.

       200-print-headings.
      * Check if the page requires a page header
           if (ws-new-page = ws-y)                 then
               move ws-page-count      to ws-page-number

      * Display Name on first page
               if (ws-page-count = ws-first-page)  then
                   write report-line   from ws-heading1-name-line
                   write report-line   from ws-heading2-title
                     after advancing ws-one line
               else
      * Display extra space if not first page
                   write report-line   from ws-blank-line
                     after advancing page
                   write report-line   from ws-heading2-title
                     
               end-if

      * Print page header and title
               write report-line       from ws-heading3-headings
                   after advancing ws-one line
               write report-line       from ws-heading4-headings
                   after advancing ws-one line
               write report-line       from spaces
                   after advancing ws-one line
           end-if.

       300-process-lines.

      * Move Known Variables to Detail Line
           move it-code        to ws-code.
           move it-transaction to ws-transaction.
           move it-store-num   to ws-store-num-edit.
           move it-invoice     to ws-invoice.
           move it-sku-code    to ws-sku-code.

      * Process the Payment Type for this Record
           perform 310-process-payment-types.
           
      * Calculate the Tax Owed for this Record
           perform 320-calc-tax.

      * Increment the Transaction Code Counts
           if (it-code-l)  then
               add ws-one      to ws-l-count
           else
               add ws-one      to ws-s-count
           end-if.
      * Dont Forget the total Record Count (Increments Regardless)
           add ws-one          to ws-rec-count.

      * Add the Transaction Amount to this Stores running total for
      *-Transactions
           perform
             varying ws-sub    from ws-one by ws-one
               until (ws-sub > ws-store-count)
       
      *        Only Add the Transaction if the Store Numbers Match
               if (it-store-num = ws-store-num(ws-sub)) then
                   add it-transaction
                               to ws-store-trans(ws-sub)
               end-if

           end-perform.
           

      * Write the Details to the Report, followed by a Blank Line
           write report-line   from ws-detail-line.
           write report-line   from ws-blank-line.

      * Read the next Record
           perform 010-read-file.

       310-process-payment-types.

      * Check for the Payment Type, assigning the correct one to the
      *-Report Detail-Line and incrementing the Types Count
           if (it-ca) then
               move ws-cash    to ws-payment-type
               add ws-one      to ws-type-ca-count
           else
           if (it-cr) then
               move ws-credit  to ws-payment-type
               add ws-one      to ws-type-cr-count
           else
           if (it-db) then
                move ws-debit  to ws-payment-type
                add ws-one     to ws-type-db-count
           end-if
           end-if
           end-if.
       320-calc-tax.
      * Calculate the Tax as Transaction * TaxationPercentage
           compute ws-tax rounded = (it-transaction * ws-the-tax).

      * Move Tax Amount to detail - Add to the Tax Amount Running Total
           move ws-tax         to ws-tax-edit.
           add ws-tax          to ws-tax-running-total.

       400-report-summary.

      * Move Count/Running-Total Variables to their Totals
           move ws-type-ca-count to ws-total-ca.
           move ws-type-cr-count to ws-total-cr.
           move ws-type-db-count to ws-total-db.
           move ws-rec-count to ws-total-recs.
           move ws-s-count to ws-total-s-recs.
           move ws-l-count to ws-total-l-recs.
           move ws-tax-running-total to ws-total-tax.

      * Calculate the Payment Type Percentages based on Total Records
           perform 410-calculate-percentages.

      * Find out which Stores accrued the highest and lowest Transaction
      *-amounts
           perform 420-find-highest-lowest-stores.

      * Write the Summary
           perform 430-write-summary.

       410-calculate-percentages.

      * Calculate Percentage of Cash Transactions    
           compute ws-ca-total-perc  rounded =
             (ws-type-ca-count / ws-rec-count).
           multiply ws-ca-total-perc by ws-perc-modifier
             giving ws-ca-total-perc.
           move ws-ca-total-perc to ws-perc-ca.

      * Calculate Percentage of Credit Transactions   
           compute ws-cr-total-perc  rounded =
             (ws-type-cr-count / ws-rec-count).
           multiply ws-cr-total-perc by ws-perc-modifier
             giving ws-cr-total-perc.
           move ws-cr-total-perc to ws-perc-cr.

      * Calculate Percentage of Debit Transactions   
           compute ws-db-total-perc  rounded =
             (ws-type-db-count / ws-rec-count).
           multiply ws-db-total-perc by ws-perc-modifier
             giving ws-db-total-perc.
           move ws-db-total-perc to ws-perc-db.

       420-find-highest-lowest-stores.
           
      * Assign an Initial Value using the Cumulative Transaction Amount
      *-of Store 1
           move ws-store-trans(ws-one) to ws-current-highest.
           move ws-store-trans(ws-one) to ws-current-lowest.

      * Loop through each store, comparing their Total Transaction
      *-Amounts to find the highest and lowest scoring Stores.
           perform
             varying ws-sub from ws-one by ws-one
             until (ws-sub > ws-store-count)
      *        Check if the current Store's Amount is Greater than the
      *        -current stored Highest Transaction Amount.
               if (ws-store-trans(ws-sub) > ws-current-highest) then
                   move ws-store-trans(ws-sub) to ws-current-highest
                   move ws-store-num(ws-sub) to ws-store-highest
               end-if
      *        Check if the current Store's Amount is Less than the
      *        -current stored Lowest Transaction Amount.
               if (ws-store-trans(ws-sub) < ws-current-lowest) then
                   move ws-store-trans(ws-sub) to ws-current-lowest
                   move ws-store-num(ws-sub) to ws-store-lowest
               end-if

           end-perform.

       430-write-summary.
      * Write Each Line of the Summary
           write report-line from ws-summary-header.
           write report-line from ws-summary-line1
             after advancing 1 line.

           write report-line from ws-summary-line2.
           write report-line from ws-summary-line3.

           write report-line from ws-summary-line4
             after advancing 1 line.
           write report-line from ws-summary-line5
           write report-line from ws-summary-line6

           write report-line from ws-summary-line7
             after advancing 1 line.

           write report-line from ws-summary-line8
             after advancing 2 lines.
           write report-line from ws-summary-line9.

      ****************************************************************|
       end program SAndLProcessing.