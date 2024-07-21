      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1319.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1319.cpv  $
      *
      *   Rev 2.1   Jan 21 2000 13:53:06   T13EX44
      * 
      *
      *   Rev 2.0   Nov 29 1999 13:55:20   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1319  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    14.12.1999                                      *
      ****************************************************************
      *    VERSION   2.01                                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00073!14.12.99!2.01! NEUERSTELLUNG             ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1319                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ROOT DATEI                   *
      *                                                              *
      ****************************************************************
          01 PI-DRPA1319.
           05 PI-DRPA1319-UFKT         PIC  X(001).
            88 PI-DRPA1319-UFKT-VNR    VALUE '1'.
            88 PI-DRPA1319-UFKT-PIN    VALUE '2'.
           05 PI-DRPA1319-ROOT-KEY-1.
            10 PI-DRPA1319-P1GES       PIC  9(002).
            10 PI-DRPA1319-P1VSNR      PIC  9(015).
           05 PI-DRPA1319-ROOT-KEY-2.
            10 PI-DRPA1319-P1INTVSNR   PIC  9(015).
      *
          01 PO-DRPA1319.
           05 PO-DRPA1319-RC           PIC  X(003).
            88 PO-DRPA1319-RC-OK       VALUE 'OK '.
            88 PO-DRPA1319-RC-NOT-FOUND
                                       VALUE 'NFD'.
           05 PO-DRPA1319-ROOT-DATEN.
            10 PO-DRPA1319-P1SA        PIC  X(002).
            10 PO-DRPA1319-P1KEY.
             15 PO-DRPA1319-P1GES      PIC  X(002).
             15 PO-DRPA1319-P1VSNR     PIC  9(015).
            10 PO-DRPA1319-P1INTVSNR   PIC  9(015).
            10 PO-DRPA1319-P1SATZ.
             15 PO-DRPA1319-P1EKRIT.
              20 PO-DRPA1319-P1EPGM    PIC  X(008).
              20 PO-DRPA1319-P1EDATUM  PIC  9(007)       PACKED-DECIMAL.
              20 PO-DRPA1319-P1EZEIT   PIC  9(007)       PACKED-DECIMAL.
              20 PO-DRPA1319-P1PSNR    PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1319-P1AVERM    PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1319-P1AVERMPRZ PIC  X(001).
             15 PO-DRPA1319-P1VVERM    PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1319-P1VVERMPRZ PIC  X(001).
             15 PO-DRPA1319-P1KDBETRGV PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1319-P1NAME     PIC  X(014).
             15 PO-DRPA1319-P1VSBEGINN PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1319-P1ZAHLWEISE
                                       PIC  9(001).
             15 PO-DRPA1319-P1DDRKZ    PIC  X(001).
             15 PO-DRPA1319-P1FILLER   PIC  X(002).
             15 PO-DRPA1319-P1KDBETRANZ
                                       PIC  9(002).
             15 PO-DRPA1319-P1TABBETR  OCCURS 1 TO 12
                          DEPENDING ON PO-DRPA1319-P1KDBETRANZ.
              20 PO-DRPA1319-P1KDBETR  PIC  9(007)       PACKED-DECIMAL.
              20 PO-DRPA1319-P1GUELTAB PIC  9(007)       PACKED-DECIMAL.
              20 PO-DRPA1319-P1AENDDAT PIC  9(007)       PACKED-DECIMAL.
