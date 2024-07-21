      *****************************************************************
      *    COPY-MEMBER: DRKI2599                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2599                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI01135!26.04.12!2.1 ! neu                       ! IFR      *
230318* AKI01579!23.03.18!2.2 ! ERWEITERUNG UM TABELLEN   ! SWO      *
230318*         !        !    ! VON PSLIFE:               !          *
230318*         !        !    ! WEITERE ANZ EINGEFÜGT     !          *
      * AKI1691 !09.10.19!2.3 ! LPM: Neue Anz eingefügt   ! BLO      *
      * -------------------------------------------------------------*
      * AKI01747!16.12.19!    ! ABLAUFDATUM für TCKI0300  ! WHO      *
      *         !        !    ! mit aufgenommen           !          *
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
      *    FÜR DIE FUNKTION DRKI2599
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRKI2599.
           05  PI-DRKI2599-DATEN.
 
             10 PI-DRKI2599-VERVERTRAGSID       PIC  X(013).
             10 PI-DRKI2599-VERPARTGESELLSCHID  PIC S9(004) COMP.
             10 PI-DRKI2599-VERVERTRAGSNUMMER   PIC  X(015).
             10 PI-DRKI2599-KNDPERSONENID       PIC  X(013).
             10 PI-DRKI2599-TAGESDATUM-EUR      PIC  X(010).
             10 PI-DRKI2599-ABLAUFDATUM         PIC  X(010).
      *
       01  PO-DRKI2599.
           05  PO-DRKI2599-DATEN.
             10 PO-DRKI2599-ANZ-ADR-UPD              PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-VRB-FEHL             PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-GELVERT-LOE          PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-PERS-LOE-VP          PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-VESOBJEKT-DEL        PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-327                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-328                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-329                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-330                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-331                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-403                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-442                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-346                  PIC S9(8) COMP.
230318       10 PO-DRKI2599-ANZ-382                  PIC S9(8) COMP.
230318       10 PO-DRKI2599-ANZ-383                  PIC S9(8) COMP.
230318       10 PO-DRKI2599-ANZ-384                  PIC S9(8) COMP.
             10 PO-DRKI2599-ANZ-385                  PIC S9(8) COMP.
 
           05  PO-DRKI2599-RC                     PIC  X(003).
               88 PO-DRKI2599-RC-OK               VALUE 'OK '.
               88 PO-DRKI2599-RC-UNV              VALUE 'UNV'.
               88 PO-DRKI2599-RC-NOT-OK           VALUE 'NOK'.
      *
      ******* ENDE COPY-MEMBER **************************************