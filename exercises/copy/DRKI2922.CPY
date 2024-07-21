      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2922.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2922.CPv  $
      *
      *   Rev 2.5   Jun 19 2020 12:02:48   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.4   Jun 17 2020 10:03:20   t13ae31
      *AKI01785: G10 SUH FLEXN
      *
      *   Rev 2.0   Jun 17 2020 06:59:24 T13AE31
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2922                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2922                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         17.06.2020 / 06:59:24 / T13AE31
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI2922
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2922.
          05 PI-DRKI2922-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2922-UFKT-CODE                PIC  9(002).

          05  PI-DRKI2922-KEY.
             10 PI-DRKI2922-VERVERTRAGSNUMMER      PIC  X(015).
             10 PI-DRKI2922-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
        01  PO-DRKI2922.
           05  PO-DRKI2922-RC                     PIC  X(003).
             88 PO-DRKI2922-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2922-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2922-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2922-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2922-DATEN.

           10 PO-DRKI2922-G10EZVVERTRAGSNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVHOSTNR PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVSPARTE PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVPBRDNR PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVBEGINN PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVABLAUF PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVSTATUS PIC  X(0009)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVNVEZBTRG PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVSUMME PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVVART PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVAENDGRD PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVAENDDAT PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVNETJBTR PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVBRUTJBTR PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVNBTRZW PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVLETBEARB PIC  X(010)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVZWEIG PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVRABNETTO PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVSUAKTBTR PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVREOBPRDNR PIC  X(0005)
                                   OCCURS 100.
           10 PO-DRKI2922-ZUGANG PIC  X(0032)
                                   OCCURS 100.
           10 PO-DRKI2922-VON PIC  X(0032)
                                   OCCURS 100.
           10 PO-DRKI2922-BIS PIC  X(0032)
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVPRODBEZ PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2922-NI-G10EZVPRODBEZ PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVFLEXNSP PIC S9(9)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2922-NI-G10EZVFLEXNSP PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2922-G10EZVFLEXNSPGW PIC  X(0001)
                                   OCCURS 100.
           10 PO-DRKI2922-NI-G10EZVFLEXNSPGW PIC S9(4) COMP
                                   OCCURS 100.

      *
      ******* ENDE COPY-MEMBER ***************************************


