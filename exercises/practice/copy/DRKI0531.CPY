      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0531.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0531.CPv  $
      *
      *   Rev 2.3   Apr 30 2009 08:17:48   t13ex14
      *AKI00805: OZP-Objekte
      *
      *   Rev 2.2   Mar 26 2009 09:41:16   t13ex14
      *AKI00731: Kfz-Versicherungsbedingungen
      *
      *   Rev 2.1   Apr 19 2000 13:23:06   T13EX41
      *AKI00065
      *- vergrößerung Schlüsselfelder
      *
      *   Rev 2.0   Feb 27 1999 12:57:48   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   19 Feb 1999 10:22:32   I13PC48
      *SELECT FUER KFZ AUF TAB. 344
      *
      *   Rev 1.0   07 Aug 1998 14:45:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0529                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0529                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   erzeugt mit genOZP - (c) PMSmicado  07.08.98/14.34.54
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI00065 !30.03.00!    !-VBBVERSBEDINGUNGID IST    !C. HEINECK*
      *         !        !    ! JETZT INTEGER             !          *
150109*AKI00731 !15.01.09!    !VBBVERSBED-TXT auf Länge 50! WMI      *
      *         !        !    !geändert                   !          *
140409*AKI00805 !14.04.09!    !neues Feld VBBBEDINGUNGKEY ! WMI      *
      *         !        !    !                           !          *
      *--------!---------------------------------------!---------!-----*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI0531
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT MIT BEZ-KFZ <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0531.
           05  PI-DRKI0531-KEY.
000330       10 PI-DRKI0531-VBBVERSBEDINGUNGID   PIC S9(9) COMP.
             10 PI-DRKI0531-VBBVERSTARLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0531-VBBVERTRAGSID   PIC X(13).
           05  PI-DRKI0531-DATEN.
140409       10 PI-DRKI0531-VBBBEDINGUNGKEY      PIC  X(2).
             10 PI-DRKI0531-VBBVERSBEDINGUNG.
                15 PI-DRKI0531-VBBVERSBED-LN     PIC S9(4) COMP.
150109*         15 PI-DRKI0531-VBBVERSBED-TXT     PIC X(10).
150109          15 PI-DRKI0531-VBBVERSBED-TXT     PIC X(50).


      *
       01  PO-DRKI0531.
           05  PO-DRKI0531-RC                 PIC  X(003).
               88 PO-DRKI0531-RC-OK                        VALUE 'OK '.
               88 PO-DRKI0531-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRKI0531-DATEN.
             10 PO-DRKI0531-VBBVERSBEDINGLFDNR   PIC S9(4) COMP.

      *
      ******* ENDE COPY-MEMBER **************************************
