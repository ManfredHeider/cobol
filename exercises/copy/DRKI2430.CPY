      *****************************************************************
      *    COPY-MEMBER: DRKI2430                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2430                    *
      *    von LIST-OZP ZXKI0313
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.10.2010 / 11:13:11 / T13EX51
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !      AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00829!20.10.10!     NEU                        ! WHO      *
      * -------------------------------------------------------------*
      * AKI01040!12.04.11!     NEUES ATTRIBUT             ! MDI      *
      *         !        !     GHAKUMBARAUSBR             !          *
      *         !        !                                !          *
140613*--------------------------------------------------------------*
140613* AKI01228!14.05.13!      zusätzliche Attribute zu  ! WHO      *
      *         !        !      Liquidation OIF und Ant-  !          *
      *         !        !      eilsrückgab OIF           !          *
      *--------------------------------------------------------------*
061014* AKI01327!06.10.14! Erweiterungen für              ! WHO      *
      *         !        ! Wertentwicklung                !          *
      *--------------------------------------------------------------*
070317* FKI01543!07.03.17! NULL-INDIKATOREN FÜR           ! WHO      *
      *         !        ! KTY/KNR Attribute mit aufgenom-!          *
      *         !        ! men                            !          *
      *         !        !                                !          *
      *DBO-10080!09.08.22!Feldlängenerweiterung für       !          *
      *         !        !GHAGUTHABENANT S9(7)V9(4) COMP-3!u131774   *

      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2430
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2430.
           05  PI-DRKI2430-PROGRAMM             PIC  X(008).
           05  PI-DRKI2430-UFKT                 PIC  X(002).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05  PI-DRKI2430-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI2430-GHAFONDID   PIC S9(4) COMP.
             10 PI-DRKI2430-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI2430-GHALFDNR   PIC S9(4) COMP.
             10 PI-DRKI2430-GHAVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI2430.
           05  PO-DRKI2430-RC                     PIC  X(003).
               88 PO-DRKI2430-RC-OK               VALUE 'OK '.
               88 PO-DRKI2430-RC-NOT-FOUND        VALUE 'NFD'.
               88 PO-DRKI2430-RC-EOF              VALUE 'EOF'.

          05 PO-DRKI2439-KEY.
             10 PO-DRKI2430-GHAVERTRAGSID        PIC X(13).
             10 PO-DRKI2430-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PO-DRKI2430-GHAFONDID            PIC S9(4) COMP.
             10 PO-DRKI2430-GHALFDNR             PIC S9(4) COMP.
      *
           05  PO-DRKI2430-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI2430-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAFLAGEINSTKURS   PIC  X(0001).
             10 PO-DRKI2430-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.
             10 PO-DRKI2430-GHAGUTHABENVORJ   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAGUTHABENWKZ   PIC  X(0001).
             10 PO-DRKI2430-GHAHISAUSZAHLUNG   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2430-GHAHISEINZAHLUNG   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2430-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.
             10 PO-DRKI2430-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.
             10 PO-DRKI2430-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.
             10 PO-DRKI2430-GHALIEFERDATUM   PIC  X(010).
             10 PO-DRKI2430-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.
120411       10 PO-DRKI2430-GHAKUMBARAUSBR   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI2430-LETZTEAENDERUNG   PIC  X(026).
             10 Po-DRKI2430-GHAANTGESPERRT1   PIC S9(7)V9(4) COMP-3.
             10 Po-DRKI2430-GHAANTGESPERRT2   PIC S9(7)V9(4) COMP-3.
             10 Po-DRKI2430-GHAANTGESPERRT3   PIC S9(7)V9(4) COMP-3.
             10 Po-DRKI2430-GHAANTEILEFREI   PIC S9(7)V9(4) COMP-3.
             10 Po-DRKI2430-GHAANTEILEVORJ   PIC S9(6)V9(4) COMP-3.
             10 Po-DRKI2430-GHAVLSPERRDAT1   PIC  X(010).
             10 Po-DRKI2430-GHAVLSPERRDAT2   PIC  X(010).
             10 Po-DRKI2430-GHAVLSPERRDAT3   PIC  X(010).
>>           10 Po-DRKI2430-GHARUECKBRUGES       PIC S9(9)V9(2)  COMP-3.
             10 Po-DRKI2430-GHARUECKBRUVORJ      PIC S9(9)V9(2)  COMP-3.
             10 Po-DRKI2430-GHAKUMBARAUSBRVJ     PIC S9(9)V9(2)  COMP-3.
             10 Po-DRKI2430-GHAANTEILGEKOIF      PIC S9(7)V9(4)  COMP-3.
             10 Po-DRKI2430-GHAKUMAUSZHALBJ      PIC S9(9)V9(2)  COMP-3.
<<           10 Po-DRKI2430-GHAANTEILVEFOIF      PIC S9(7)V9(4)  COMP-3.
140612       10 Po-DRKI2430-GHAANTEILGESPOIF     PIC S9(7)V9(4)  COMP-3.
             10 Po-DRKI2430-GHAANTERWVORJU13     PIC S9(7)V9(4) COMP-3.
061014       10 Po-DRKI2430-GHAANLEGERIDOIF      PIC  X(0014).
>>           10 Po-DRKI2430-GHAWPBEZ-KTY         PIC S9(4) COMP.
070317       10 Po-DRKI2430-NI-GHAWPBEZ-KTY      PIC S9(4) COMP.
             10 Po-DRKI2430-GHAWPBEZ-KNR         PIC S9(15)V COMP-3.
070317       10 Po-DRKI2430-NI-GHAWPBEZ-KNR      PIC S9(4) COMP.
             10 Po-DRKI2430-GHARUECKNAHMEPR      PIC S9(5)V9(4) COMP-3.
             10 Po-DRKI2430-GHANRGWINKLBBA       PIC S9(11)V9(2) COMP-3.
             10 Po-DRKI2430-GHANRGWINKLBBAVH     PIC S9(11)V9(2) COMP-3.
<<           10 Po-DRKI2430-GHANRGWOHNEBBA       PIC S9(11)V9(2) COMP-3.
061014       10 Po-DRKI2430-GHANRGWOHNEBBAVH     PIC S9(11)V9(2) COMP-3.
      *
      ******* ENDE COPY-MEMBER ***************************************







