      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1543.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI1543.CPv  $
      *
      *   Rev 2.3   Nov 24 2020 09:03:04   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.2   Apr 27 2005 14:27:02   t13ex39
      *AKI00249: ART DES DEPOTS
      *
      *   Rev 2.0   Jan 13 2003 15:52:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI1543                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1543                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:40 / U130685
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
      *    FÜR DIE FUNKTION DRKI1543
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI1543.
           05  PI-DRKI1543-KEY.


ANF   *        %M_PI_KEY%
             10 PI-DRKI1543-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI1543-GHPVERTRAGSID   PIC  X(0013).
END   *

      *
       01  PO-DRKI1543.
           05  PO-DRKI1543-RC                     PIC  X(003).
               88 PO-DRKI1543-RC-OK               VALUE 'OK '.
               88 PO-DRKI1543-RC-NOT-FOUND        VALUE 'NFD'.
      *
           05  PO-DRKI1543-NI.
               10 NI-DRKI1543-TAB.
                  15 DRKI1543-ELE
                     PIC S9(04) COMP OCCURS 1 TIMES.
               10 NI-DRKI1543 REDEFINES NI-DRKI1543-TAB.

ANF   *        %M_PO_DATEN_NI%
             15 PO-DRKI1543-NI-GHPRISIKOKLASSE   PIC S9(4) COMP.
END   *

           05  PO-DRKI1543-DATEN.


ANF   *        %M_PO_DATEN%
             10 PO-DRKI1543-GHPABSCHLUSSVBNR   PIC S9(9) COMP.
             10 PO-DRKI1543-GHPANLAGENAME.
                49 PO-DRKI1543-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PO-DRKI1543-GHPANLAGENAME-TEXT PIC X(15).
             10 PO-DRKI1543-GHPANLAGEZIEL   PIC  X(0001).
             10 PO-DRKI1543-GHPDEPOTART   PIC  X(0001).
             10 PO-DRKI1543-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1543-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PO-DRKI1543-GHPESAFE   PIC  X(0001).
             10 PO-DRKI1543-GHPFAXREVERS   PIC  X(0001).
             10 PO-DRKI1543-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1543-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PO-DRKI1543-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1543-GHPGUTHABENWKZ   PIC  X(0001).
             10 PO-DRKI1543-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PO-DRKI1543-GHPINFOSPERREDATUM   PIC  X(010).
             10 PO-DRKI1543-GHPINFOWPERREKZ   PIC  X(0001).
             10 PO-DRKI1543-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PO-DRKI1543-GHPONLINEREVERS   PIC  X(0001).
             10 PO-DRKI1543-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI1543-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PO-DRKI1543-GHPPRODUKTID   PIC S9(4) COMP.
             10 PO-DRKI1543-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PO-DRKI1543-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1543-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PO-DRKI1543-GHPSCHLIESSDATUM   PIC  X(010).
             10 PO-DRKI1543-GHPSUBTYPE   PIC  X(0001).
             10 PO-DRKI1543-GHPTELEFONREVERS   PIC  X(0001).
             10 PO-DRKI1543-GHPVERFUEGUNGID   PIC  X(0002).
             10 PO-DRKI1543-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PO-DRKI1543-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PO-DRKI1543-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *


      *
      ******* ENDE COPY-MEMBER ***************************************
