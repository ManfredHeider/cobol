      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1819.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1819.CPv  $
      *
      *   Rev 2.5   Mar 15 2017 14:30:32   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.4   16 Dec 2008 11:35:24   t13ex27
      *AKI00767: CR 14112 - Historiesierung Riesterdaten-DB-Änderung
      *
      *   Rev 2.3   16 Dec 2008 11:17:10   t13ex27
      *AKI00767: CR 14112 - Historiesierung Riesterdaten-DB-Änderung
      *
      *   Rev 2.2   09 Jun 2008 15:58:24   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben OZPs
      *
      *   Rev 2.1   Sep 20 2005 08:43:56   t13ex49
      *AKI00483: Riester Förderung - OPC-Änderungen - BTGVERF - OZP
      *
      *   Rev 2.0   Jun 23 2005 10:38:12   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1819                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1819                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.02.2017 / 16:28:46 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI1819
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1819.
           05  PI-DRKI1819-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1819-RZLVERTRAGSID   PIC  X(0013).
             10 PI-DRKI1819-RZLZULAGENJAHR   PIC S9(4) COMP.
END   *

      *
       01  PO-DRKI1819.
           05  PO-DRKI1819-RC                     PIC  X(003).
               88 PO-DRKI1819-RC-OK               VALUE 'OK '.
               88 PO-DRKI1819-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1819-NI.
               10 NI-DRKI1819-TAB.
                  15 DRKI1819-ELE
                     PIC S9(04) COMP OCCURS 4 TIMES.
               10 NI-DRKI1819 REDEFINES NI-DRKI1819-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI1819-NI-RZLANSPRUCH-KNR   PIC S9(4) COMP.
             15 PO-DRKI1819-NI-RZLANSPRUCH-KTY   PIC S9(4) COMP.
             15 PO-DRKI1819-NI-RZLKUERZUNG-KNR   PIC S9(4) COMP.
             15 PO-DRKI1819-NI-RZLKUERZUNG-KTY   PIC S9(4) COMP.
END   *

           05  PO-DRKI1819-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1819-RZLANSPRUCH-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI1819-RZLANSPRUCH-KTY   PIC S9(4) COMP.
             10 PO-DRKI1819-RZLDATUMZULAGE   PIC  X(010).
             10 PO-DRKI1819-RZLKUERZUNG-KNR   PIC S9(15)V COMP-3.
             10 PO-DRKI1819-RZLKUERZUNG-KTY   PIC S9(4) COMP.
             10 PO-DRKI1819-RZLZULAGEBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1819-RZLZULAGEBETRAGWKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************

