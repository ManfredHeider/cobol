      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1542.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1542.CPv  $
      *
      *   Rev 2.3   Nov 24 2020 09:02:50   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.2   Apr 27 2005 14:26:56   t13ex39
      *AKI00249: ART DES DEPOTS
      *
      *   Rev 2.0   Jan 13 2003 15:52:06   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1542                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1542                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:38 / U130685
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
      *    FÜR DIE FUNKTION DRKI1542
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  UPDATE  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1542.
           05  PI-DRKI1542-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1542-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1542-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI1542-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI1542-NI-GHPRISIKOKLASSE  PIC S9(4) COMP.
END   *

           05  PI-DRKI1542-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI1542-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PI-DRKI1542-GHPANLAGENAME.
                49 PI-DRKI1542-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI1542-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI1542-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI1542-GHPDEPOTART   PIC  X(0001).
             10 PI-DRKI1542-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1542-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI1542-GHPESAFE   PIC  X(0001).
             10 PI-DRKI1542-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI1542-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1542-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI1542-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1542-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI1542-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI1542-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI1542-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI1542-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI1542-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI1542-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI1542-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI1542-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI1542-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI1542-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1542-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI1542-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI1542-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI1542-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI1542-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI1542-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI1542-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI1542-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI1542-NI1.
           05 PI-DRKI1542-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                  OCCURS 1.
       01  PI-DRKI1542-NI7.
           05 PI-DRKI1542-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                  OCCURS 1.
      *
       01  PO-DRKI1542.
           05  PO-DRKI1542-RC                     PIC  X(003).
               88 PO-DRKI1542-RC-OK               VALUE 'OK '.
               88 PO-DRKI1542-RC-NOT-FOUND        VALUE 'NFD'.
      *
      ******* ENDE COPY-MEMBER ***************************************
