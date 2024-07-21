      *****************************************************************
      *    COPY-MEMBER: DRKI3963                                       
      *                                                                
      *    CURSOR FULL SELECT ZXKI0560 G79_GOLDRENTE                       
      *                                                                
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER           
      *    FÜR DIE FUNKTION DRKI3963                                   
      *    AUSSER: DEM FUNKTIONSNAMEN,                                 
      *            DEM ERR-ERROR-BEREICH UND                           
      *            DEM STANDARD-STATUS-BEREICH                         
      *            (JEWEILS SEPARATE COPYS)                            
      *                                                                
      *    FUNKTION: LIST                                              
      *                                                                
      *****************************************************************
      ****************************************************************
      * LOGBUCH:                                                     *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME    *          
      * -------------------------------------------------------------*          
      * DBO-10295  !17.08.22! NEUERSTELLUNG FUER NEUE TT   ! ATAR    *
      *            !        ! G79_GOLDRENTE                !         *          
      *            !        !                              !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRKI3963.                                           
           05 PI-DRKI3963-UFKT-CODE         PIC  9(0002).     
             88 PI-DRKI3963-UFKT-FIRST           VALUE 1.      
             88 PI-DRKI3963-UFKT-NEXT            VALUE 2.      
             88 PI-DRKI3963-UFKT-QUIT            VALUE 3.      
                                                           
           05 PI-DRKI3963-KEY.                                    
             10 PI-DRKI3963-G79MANDANT     PIC S9(009) COMP.
                                                           
       01  PO-DRKI3963.                                           
           05 PO-DRKI3963-RC               PIC X(0003).     
             88 PO-DRKI3963-RC-OK              VALUE 'OK '.       
             88 PO-DRKI3963-RC-LEER            VALUE 'LER'.       
             88 PO-DRKI3963-RC-EOF             VALUE 'EOF'.       
                                                           
           05 PO-DRKI3963-KEY.                                    
             10 PO-DRKI3963-G79GRVNR           PIC X(15).
             10 PO-DRKI3963-G79GRSPARRATEID    PIC S9(9) COMP. 

           05  PO-DRKI3963-DATEN.      
              10 PO-DRKI3963-G79GRMANDANT      PIC S9(9) COMP.
              10 PO-DRKI3963-G79GRISIN         PIC X(12).            
              10 PO-DRKI3963-G79GRSTATUS       PIC X(1).         
              10 PO-DRKI3963-G79GRBEGINNDAT    PIC X(8).           
              10 PO-DRKI3963-G79GRENDEDAT      PIC X(8).           
              10 PO-DRKI3963-G79GRABSCHLVBNR   PIC S9(9) COMP.  
              10 PO-DRKI3963-G79GRRABATTKZN    PIC X(1).             
              10 PO-DRKI3963-G79GRKNDRABATT    PIC X(1).                
              10 PO-DRKI3963-G79GRKUNDENIDEXT  PIC S9(9) COMP.  
              10 PO-DRKI3963-G79GRFLATRATE     PIC X(1).              
              10 PO-DRKI3963-G79GRKUNDENDATEN.                               
                 49 PO-DRKI3963-G79GRKNDNDTN-LEN PIC S9(4) COMP.
                 49 PO-DRKI3963-G79GRKNDNDTN-TEXT PIC X(382).          
              10 PO-DRKI3963-G79GRSPARRATE     PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3963-G79GRZAHLUNGSART  PIC X(1).             
              10 PO-DRKI3963-G79GRABBUCHTAG    PIC X(1).               
              10 PO-DRKI3963-G79GRIBAN         PIC X(34).               
              10 PO-DRKI3963-G79GRBIC          PIC X(11).               
              10 PO-DRKI3963-G79GRABWNAME      PIC X(40).                
              10 PO-DRKI3963-G79GRRATEOEFFDAT  PIC X(8).                 
              10 PO-DRKI3963-G79GRENDEAUSDAT   PIC X(8).               
              10 PO-DRKI3963-G79GRRATESCHLDAT  PIC X(8).               
              10 PO-DRKI3963-G79GRDYNAMIK      PIC S9(3)V9(2) COMP-3.
              10 PO-DRKI3963-G79GRSPARRATELFDNR
                                               PIC S9(4) COMP.    
              10 PO-DRKI3963-G79GRBESTGRAMM    PIC S9(7)V9(4) COMP-3.
              10 PO-DRKI3963-G79GRBESTEURO     PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3963-G79GRBUCHUNGSDAT  PIC X(0008).                
              10 PO-DRKI3963-G79GRRUECKGRAMM   PIC S9(4)V9(2) COMP-3.
              10 PO-DRKI3963-G79GRSUMMEEIN     PIC S9(7)V9(2) COMP-3.
              10 PO-DRKI3963-G79GRSUMMEAUS     PIC S9(7)V9(2)  COMP-3.
              10 PO-DRKI3963-LETZTERAENDERER   PIC X(10).            
      *
      ******* ENDE COPY-MEMBER **************************************