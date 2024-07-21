      *****************************************************************
      *    COPY-MEMBER: DRKI2512                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2512                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
230318* AKI01579!23.03.18!2.2 ! ERWEITERUNG UM TABELLEN   ! SWO      *
230318*         !        !    ! VON PSLIFE:               !          *
230318*         !        !    ! WEITERE COUNTER EINGEFÜGT !          *
      * AKI01691!04.10.19!2.3 ! ERWEITERUNG UM LPMVERTRAG,! BLO      *
      *         !        !    ! VERTRAGSNUMMER, PARTGES.  !          *
      ***************************************************************** 
      * AENDERUNGEN:                                                    
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-8439   !16.06.21! ANZ LPMVERTRAG RAUS:         ! T13AC20  
      *            !        !  - PO-DRKI2512-COUNTER-200   !          
      *            !        ! ANZ LBNPSZAHLUNG REIN:       !          
      *            !        !  - PO-DRKI2512-COUNTER-385   !          
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2512
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2512.
           05  PI-DRKI2512-KEY.
 
             10 PI-DRKI2512-VERTRAGSID       PIC  X(0013).
             10 PI-DRKI2512-VERTRAGSNUMMER   PIC  X(0015).
             10 PI-DRKI2512-PARTGESELLSCHID  PIC S9(004) COMP.
 
      *
       01  PO-DRKI2512.
           05  PO-DRKI2512-RC                     PIC  X(003).
               88 PO-DRKI2512-RC-OK               VALUE 'OK '.
               88 PO-DRKI2512-RC-NOK              VALUE 'NOK'.
 
           05  PO-DRKI2512-DATEN.
 
             10   PO-DRKI2512-COUNTER-327       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-328       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-329       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-330       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-331       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-403       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-442       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-346       PIC S9(8) COMP.
230318       10   PO-DRKI2512-COUNTER-382       PIC S9(8) COMP.
230318       10   PO-DRKI2512-COUNTER-383       PIC S9(8) COMP.
230318       10   PO-DRKI2512-COUNTER-384       PIC S9(8) COMP.
             10   PO-DRKI2512-COUNTER-385       PIC S9(8) COMP.
      *
      ******* ENDE COPY-MEMBER **************************************