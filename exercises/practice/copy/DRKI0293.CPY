      *---------------------------------------------------------------*
      *    Standard f r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0293.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0293.CPv  $
      *
      *   Rev 2.4   Nov 24 2020 09:02:16   u130685
      *AKI01805: Guthabenprodukt OZP/Copy
      *
      *   Rev 2.3   Jan 27 2003 10:55:02   t13ex39
      *AKI00162 DEUTSCHE BANK Neue Felder
      *
      *   Rev 1.0   07 Jul 1998 11:50:24   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRKI0293                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0293                    *
      *    --------------------------------------------------------   *
      ****************************************************************
      *   ERZEUGT MIT OZPGEN         13.11.2020 / 09:18:23 / U130685
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
      *    FÜR DIE FUNKTION DRKI0293
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  INSERT  <<<
      *
      ****************************************************************
      *
       01  PI-DRKI0293.
           05  PI-DRKI0293-KEY.

ANF   *        %M_PI_KEY%
             10 PI-DRKI0293-GHPGUTHABPRODLFDNR   PIC S9(4) COMP.
             10 PI-DRKI0293-GHPVERTRAGSID   PIC  X(0013).
END   *

           05  PI-DRKI0293-NI.

ANF   *        %M_PI_DATEN_NI%
             15 PI-DRKI0293-NI-GHPRISIKOKLASSE  PIC S9(4) COMP.
END   *

           05  PI-DRKI0293-DATEN.

ANF   *        %M_PI_DATEN%
             10 PI-DRKI0293-GHPANLAGENAME.
                49 PI-DRKI0293-GHPANLAGENAME-LEN PIC S9(4) COMP.
                49 PI-DRKI0293-GHPANLAGENAME-TEXT PIC X(15).
             10 PI-DRKI0293-GHPANLAGEZIEL   PIC  X(0001).
             10 PI-DRKI0293-GHPEIGENAUFWAND   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0293-GHPEIGENVERMITTELT   PIC  X(0001).
             10 PI-DRKI0293-GHPESAFE   PIC  X(0001).
             10 PI-DRKI0293-GHPFAXREVERS   PIC  X(0001).
             10 PI-DRKI0293-GHPGUTHABEN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0293-GHPGUTHABENANT   PIC S9(6)V9(3) COMP-3.
             10 PI-DRKI0293-GHPGUTHABENWERT   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0293-GHPGUTHABENWKZ   PIC  X(0001).
             10 PI-DRKI0293-GHPGUTHWERTWKZ   PIC  X(0001).
             10 PI-DRKI0293-GHPINFOSPERREDATUM   PIC  X(010).
             10 PI-DRKI0293-GHPINFOWPERREKZ   PIC  X(0001).
             10 PI-DRKI0293-GHPOFFNUNGSDATUM   PIC  X(010).
             10 PI-DRKI0293-GHPONLINEREVERS   PIC  X(0001).
             10 PI-DRKI0293-GHPPERFGESAMT   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI0293-GHPPERFLFDJAHR   PIC S9(13)V9(5) COMP-3.
             10 PI-DRKI0293-GHPPRODUKTID   PIC S9(4) COMP.
             10 PI-DRKI0293-GHPRISIKOKLASSE   PIC S9(4) COMP.
             10 PI-DRKI0293-GHPSCHLAUSZGESCH   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0293-GHPSCHLAUSZGWKZ   PIC  X(0001).
             10 PI-DRKI0293-GHPSCHLIESSDATUM   PIC  X(010).
             10 PI-DRKI0293-GHPSUBTYPE   PIC  X(0001).
             10 PI-DRKI0293-GHPTELEFONREVERS   PIC  X(0001).
             10 PI-DRKI0293-GHPVERFUEGUNGID   PIC  X(0002).
             10 PI-DRKI0293-GHPZINSSATZ   PIC S9(3)V9(2) COMP-3.
             10 PI-DRKI0293-GHPZWISCHENGEWINN   PIC S9(9)V9(2) COMP-3.
             10 PI-DRKI0293-GHPZWISCHENGEWWKZ   PIC  X(0001).
END   *

      *  INITIALISIERUNG NULL-INDIKATOR
       01  PI-DRKI0293-NI1.
           05 PI-DRKI0293-INIT-TAB1 PIC S9(04) COMP VALUE -1
                                      OCCURS 1.
       01  PI-DRKI0293-NI7.
           05 PI-DRKI0293-INIT-TAB7 PIC S9(04) COMP VALUE -7
                                      OCCURS 1.

      *
       01  PO-DRKI0293.
           05  PO-DRKI0293-RC                     PIC  X(003).
               88 PO-DRKI0293-RC-OK                  VALUE 'OK '.
               88 PO-DRKI0293-RC-DUPLICATE-VALUE VALUE 'DVA'.
      *
      ******* ENDE COPY-MEMBER **************************************
