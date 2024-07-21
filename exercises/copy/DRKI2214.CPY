      ****************************************************************
      *    COPY-MEMBER: DRKI2214   VARIABLEN  FÜR UPRO UPKI0210      *  
      *    -------------------------------------------------------   *  
      *    ERSTELLT: SCHLEICHER                                      *  
      *    DATUM:    23.11.2007                                      *  
      ****************************************************************  
      *    VERSION   2.0                                             *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*
      * AKI00639!23.11.07!      NEUERSTELLUNG WEIL FÜR AGI!SCHLEICHER*  
      *         !        !      3 NEUE FELDER HINZUGE-    !          *  
      *         !        !      KOMMEN SIND               !          *  
      * -------------------------------------------------------------*  
      *DBO-10227!11.08.22!Längenänderung                  !u131774   *
      *         !        !GHBGUTHABENANT von 10 auf 11 B  !          *
      *         !        !                                !          *
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
       01 DRKI2214-GES.                                                 
          05 DRKI2214-FKT                   PIC  9(02).                 
             88 DRKI2214-INSERT                  VALUE 01.              
             88 DRKI2214-READ                    VALUE 02.              
                                                                        
          05 DRKI2214-DATEN.                                            
             10 DRKI2214-GHBVERTRAGSID      PIC  X(13).                 
             10 DRKI2214-GHBGUTHABPRODLFDNR PIC S9(04) COMP.            
             10 DRKI2214-GHBFONDID          PIC S9(04) COMP.            
             10 DRKI2214-GHBGHALFDNR        PIC S9(04) COMP.            
             10 DRKI2214-GHBBUCHUNGLFDNR    PIC S9(04) COMP.            
             10 DRKI2214-GHBBETRAGLETZTBUCH PIC S9(09)V9(02) COMP-3.    
             10 DRKI2214-GHBBETRLBUWKZ      PIC  X(01).                 
             10 DRKI2214-GHBBUCHUNGSDATUM   PIC  X(10).                 
             10 DRKI2214-GHBGUTHABENANT     PIC S9(07)V9(04) COMP-3.    
             10 DRKI2214-GHBKZFINHLETZTBUID PIC S9(04) COMP.            
             10 DRKI2214-GHBSTEUBETRLETZTBU PIC S9(09)V9(02) COMP-3.    
             10 DRKI2214-GHBSTLETZTBUWKZ    PIC  X(01).                 
             10 DRKI2214-GHBNETTOBETRAG     PIC S9(09)V9(02) COMP-3.    
             10 DRKI2214-GHBNETTOBETWKZ     PIC  X(01).                 
             10 DRKI2214-GHBGEBBETRAG       PIC S9(9)V9(2) COMP-3.      
             10 DRKI2214-GHBGEBSCHLUESSEL   PIC S9(4) COMP.             
             10 DRKI2214-GHBGEBTEXT         PIC  X(020).                
                                                                        
                                                                        
          05 DRKI2214-RC                    PIC  X(03).                 
             88 DRKI2214-RC-OK                   VALUE 'OK '.           
                                                                        
          05 DRKI2214-DB-OP-Z               PIC  9(01).                 
      *---------------------------------------------------------------* 
