      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1820.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI1820.CPv  $
      *
      *   Rev 2.5   Mar 15 2017 14:30:40   t13ex27
      *AKI01492: AML-Änderungen zum RT 03/2017OZPs
      *
      *   Rev 2.4   16 Dec 2008 11:35:28   t13ex27
      *AKI00767: CR 14112 - Historiesierung Riesterdaten-DB-Änderung
      *
      *   Rev 2.3   16 Dec 2008 11:17:14   t13ex27
      *AKI00767: CR 14112 - Historiesierung Riesterdaten-DB-Änderung
      *
      *   Rev 2.2   09 Jun 2008 15:58:28   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben OZPs
      *
      *   Rev 2.1   Sep 20 2005 08:44:12   t13ex49
      *AKI00483: Riester Förderung - OPC-Änderungen - BTGVERF - OZP
      *
      *   Rev 2.0   Jun 23 2005 10:56:40   t13ex49
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1820                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1820                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         14.02.2017 / 16:28:49 / T13EX27
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
      *    FÜR DIE FUNKTION DRKI1820
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1820.
           05  PI-DRKI1820-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI1820-RZLVERTRAGSID   PIC  X(0013).
             10 PI-DRKI1820-RZLZULAGENJAHR   PIC S9(4) COMP.
END   *

           05  PI-DRKI1820-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI1820-NI-RZLANSPRUCH-KNR PIC S9(4) COMP.
             15 PI-DRKI1820-NI-RZLANSPRUCH-KTY PIC S9(4) COMP.
             15 PI-DRKI1820-NI-RZLKUERZUNG-KNR PIC S9(4) COMP.
             15 PI-DRKI1820-NI-RZLKUERZUNG-KTY PIC S9(4) COMP.
END   *

           05  PI-DRKI1820-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI1820-RZLANSPRUCH-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI1820-RZLANSPRUCH-KTY   PIC S9(4) COMP.
             10 PI-DRKI1820-RZLDATUMZULAGE   PIC  X(010).
             10 PI-DRKI1820-RZLKUERZUNG-KNR   PIC S9(15)V COMP-3.
             10 PI-DRKI1820-RZLKUERZUNG-KTY   PIC S9(4) COMP.
             10 PI-DRKI1820-RZLZULAGEBETRAG   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1820-RZLZULAGEBETRAGWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI1820-NI1.
           05 PI-DRKI1820-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 4.
       01  PI-DRKI1820-NI7.
           05 PI-DRKI1820-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 4.

      *
       01  PO-DRKI1820.
           05  PO-DRKI1820-RC                     PIC  X(003).
               88 PO-DRKI1820-RC-OK                  VALUE 'OK '.
               88 PO-DRKI1820-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************

