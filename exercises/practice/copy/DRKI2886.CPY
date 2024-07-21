      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.04.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2886.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2886.CPv  $
      *
      *   Rev 2.3   Mar 18 2015 15:11:22   t13ex78
      *AKI01295: Neue Wohngebäudetarif zum 1.1.2015 MultiRow OZP ZM
      *KI0429
      *
      *   Rev 2.2   Oct 08 2014 13:42:04   t13ex78
      *AKI01290: SUH KSDS in DB/2 - Copy-Strecken für Neuprogramme
      *und Änderungen
      *
      *   Rev 2.1   Sep 01 2014 11:03:46   t13ex78
      *AKI01290: SUH KSDS in DB/2 - neue Block-List-OZP's
      *
      *   Rev 2.0   Apr 15 2014 12:39:47 T13EX78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI2886                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2886                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         10.02.2015 / 07:49:09 / T13EX78
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
      *    FÜR DIE FUNKTION DRKI2886
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MULTI ROW  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI2886.
          05 PI-DRKI2886-PROGRAMM                 PIC  X(008).
          05 PI-DRKI2886-UFKT-CODE                PIC  9(002).
 
          05  PI-DRKI2886-KEY.
            10 PI-DRKI2886-VERVERTRAGSNUMMER      PIC  X(015).
            10 PI-DRKI2886-VERPARTGESELLSCHID PIC S9(4) USAGE COMP.
 
      *
       01  PO-DRKI2886.
           05  PO-DRKI2886-RC                     PIC  X(003).
             88 PO-DRKI2886-RC-OK                        VALUE 'OK '.
             88 PO-DRKI2886-RC-LEER                      VALUE 'LER'.
             88 PO-DRKI2886-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRKI2886-ROWS-FETCHED           PIC S9(9) COMP.
      *
           05  PO-DRKI2886-DATEN.
 
           10 PO-DRKI2886-VRIVERTRAGSID PIC  X(0013)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIVETLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRISIKOGRUPPID PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRITHEMGEBSL-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRITHEMGEBSL-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRISIKOLFDNR PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRIEINZVTGHOSTID PIC  X(0015)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKKENNZIFFER PIC  X(0010)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKNAME PIC  X(0050)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKBETRAG PIC S9(7)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKSUMME PIC S9(9)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKENTSCHGREN PIC S9(8)V9(2) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKENTGRZ-KTY PIC S9(4) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKENTGRZ-KNR PIC S9(15)V COMP-3
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKMENGE PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKMENGEEINH PIC  X(0025)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKSATZ PIC S9(3)V9(7) COMP-3
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKSATZEINH PIC S9(9) COMP
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKTEXT PIC  X(0030)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKMITVERSKZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKSUMMEEINH PIC  X(0020)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIRSKSUMUNBEGRZ PIC  X(0002)
                                   OCCURS 100.
           10 PO-DRKI2886-LETZTEAENDERUNG PIC  X(026)
                                   OCCURS 100.
           10 PO-DRKI2886-VRIMERKMAL PIC S9(9)V COMP-3
                                   OCCURS 100.
 
      *
      ******* ENDE COPY-MEMBER ***************************************

