      *****************************************************************         
CCC005*    EXEC SQL DECLARE VERTRAGZAHLUNGKU TABLE                              
CCC004*    ( VZKVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      VZKLAUFMONAT                   DECIMAL(6, 0) NOT NULL,             
CCC004*      VZKZAHLBETRAG                  DECIMAL(13, 2),                     
CCC004*      VZKZAHLBETR_NETTO              DECIMAL(13, 2) NOT NULL             
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VERTRAGZAHLUNGKU            *               
      ******************************************************************        
       01  VERTRAGZAHLUNGKU-GESAMT.                                             
        05 NI-VERTRAGZAHLUNGKU-TAB.                                             
           10 NI-VERTRAGZAHLUNGKU-ELE PIC S9(04) COMP OCCURS 4 TIMES.           
        05 NI-VERTRAGZAHLUNGKU REDEFINES NI-VERTRAGZAHLUNGKU-TAB.               
           10 NI-VZKVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-VZKLAUFMONAT       PIC S9(4) COMP.                             
           10 NI-VZKZAHLBETRAG      PIC S9(4) COMP.                             
           10 NI-VZKZAHLBETR-NETTO  PIC S9(4) COMP.                             
        05 DCL-VERTRAGZAHLUNGKU.                                                
           10 VZKVERTRAGSID        PIC X(13).                                   
           10 VZKLAUFMONAT         PIC S9(6)V USAGE COMP-3.                     
           10 VZKZAHLBETRAG        PIC S9(11)V9(2) USAGE COMP-3.                
           10 VZKZAHLBETR-NETTO    PIC S9(11)V9(2) USAGE COMP-3.                
      ************************** ENDE-DCLGEN *************************          
