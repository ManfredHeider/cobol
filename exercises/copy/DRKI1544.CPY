      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1544.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1544.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 09:03:20   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.3   Oct 16 2008 13:44:18   t13ex39
      *AKI00735: Bruttoperfomanceberechnung von AllianzGI in der KI
      *
      *   Rev 2.0   Jan 13 2003 15:52:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1544                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1544                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:42 / U130685
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      *---------!--------!--------------------------------!----------*
      * AKI01805!13.11.20!    ! NEUES ATTRIBUT ESAFE      ! ATAR     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRKI1544
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1544.
           05  PI-DRKI1544-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI1544-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1544-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI1544-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI1544-NI-GHPPERFGESAMT    PIC S9(4) COMP.
             15 PI-DRKI1544-NI-GHPPERFLFDJAHR   PIC S9(4) COMP.
             15 PI-DRKI1544-NI-GHPRISIKOKLASSE  PIC S9(4) COMP.
END   *

           05  PI-DRKI1544-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI1544-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI1544-GHPANLAGENAME.
                49 PI-DRKI1544-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI1544-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI1544-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI1544-GHPDEPOTART   PIC  X(0001).
             10 PI-DRKI1544-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1544-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI1544-GHPESAFE   PIC  X(0001).
             10 PI-DRKI1544-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI1544-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1544-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI1544-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1544-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI1544-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI1544-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI1544-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI1544-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI1544-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI1544-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI1544-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI1544-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI1544-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI1544-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1544-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI1544-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI1544-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI1544-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI1544-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI1544-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1544-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1544-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI1544-NI1.
           05 PI-DRKI1544-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 3.
       01  PI-DRKI1544-NI7.
           05 PI-DRKI1544-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 3.

      *
       01  PO-DRKI1544.
           05  PO-DRKI1544-RC                     PIC  X(003).
               88 PO-DRKI1544-RC-OK                  VALUE 'OK '.
               88 PO-DRKI1544-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
