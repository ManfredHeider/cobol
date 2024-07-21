      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1321.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1321.CPv  $
      *
      *   Rev 2.1   Jan 21 2000 13:53:00   T13EX44
      * 
      *
      *   Rev 2.0   Dec 14 1999 16:42:10   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1321  FUNKTION SELECT STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRPA1321                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ABH.SEGMENTE DATEI                   *
      *                                                              *
      ****************************************************************
          01 PI-DRPA1321.
           05 PI-DRPA1321-ABHSEG-UFKT  PIC  X(001).
            88 PI-DRPA1321-UFKT-READ-FIRST
                                       VALUE '1'.
            88 PI-DRPA1321-UFKT-READ-NEXT
                                       VALUE '2'.
           05 PI-DRPA1321-ABHSEG-KEY.
            10 PI-DRPA1321-P2GES       PIC  9(002).
            10 PI-DRPA1321-P2INTVSNR   PIC  9(015).
            10 PI-DRPA1321-P2FAELLIG   PIC  9(006).
            10 PI-DRPA1321-P2PRODSCHL
                                       PIC  9(002).
            10 PI-DRPA1321-P2LFDNR     PIC  9(002).
      *
          01 PO-DRPA1321.
           05 PO-DRPA1321-RC           PIC  X(003).
            88 PO-DRPA1321-RC-OK       VALUE 'OK '.
            88 PO-DRPA1321-RC-NOT-FOUND
                                       VALUE 'NFD'.
           05 PO-DRPA1321-ABHSEG-DATEN.
            10 PO-DRPA1321-SA          PIC  X(002).
            10 PO-DRPA1321-KEY.
             15 PO-DRPA1321-GES        PIC  X(002).
             15 PO-DRPA1321-INTVSNR    PIC  9(015).
             15 PO-DRPA1321-KFAELLIG   PIC  9(006).
             15 PO-DRPA1321-PRODSCHL   PIC  9(002).
             15 PO-DRPA1321-LFDNR      PIC  9(002).
            10 PO-DRPA1321-SATZ.
             15 PO-DRPA1321-EKRIT.
              20 PO-DRPA1321-EPGM      PIC  X(008).
              20 PO-DRPA1321-EDATUM    PIC  9(007)       PACKED-DECIMAL.
              20 PO-DRPA1321-EZEIT     PIC  9(007)       PACKED-DECIMAL.
              20 PO-DRPA1321-PSNR      PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1321-FREIKZ     PIC  X(001).
             15 PO-DRPA1321-VSNR       PIC  9(015)       PACKED-DECIMAL.
             15 PO-DRPA1321-FAELLIG    PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1321-BUCHDATPART
                                       PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1321-BUCHDATAML PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1321-TERMIN     PIC  9(007)       PACKED-DECIMAL.
             15 PO-DRPA1321-FILLER     PIC  X(017).
             15 PO-DRPA1321-BAD        REDEFINES PO-DRPA1321-FILLER.
              20 PO-DRPA1321-BAD-LAUFZEIT
                                       PIC  9(002).
              20 PO-DRPA1321-BAD-REST  PIC  X(015).
             15 PO-DRPA1321-BFG        REDEFINES PO-DRPA1321-FILLER.
              20 PO-DRPA1321-BFG-KDINH.
               25 PO-DRPA1321-BFG-KDAVERM
                                       PIC  9(008).
              20 PO-DRPA1321-BFG-KDDAT PIC  9(006).
              20 PO-DRPA1321-BFG-REST  PIC  X(003).
             15 PO-DRPA1321-ROLAND     REDEFINES PO-DRPA1321-FILLER.
              20 PO-DRPA1321-ROLFILL   PIC  X(008).
              20 PO-DRPA1321-IPBETR    PIC  9(007)V9(02) PACKED-DECIMAL.
              20 PO-DRPA1321-IPPROVS   PIC  9(002)V9(01) PACKED-DECIMAL.
              20 PO-DRPA1321-LAUFZEIT  PIC  X(002).
             15 PO-DRPA1321-GES91      REDEFINES PO-DRPA1321-FILLER.
              20 PO-DRPA1321-GES91-GRUNDPROV
                                       PIC  9(002)V9(03).
              20 PO-DRPA1321-GES91-REST
                                       PIC  X(012).
             15 PO-DRPA1321-LFDNRE     PIC  9(002).
             15 PO-DRPA1321-PRODART    PIC  9(001).
             15 PO-DRPA1321-PROVPFLSU  PIC S9(007)V9(02) PACKED-DECIMAL.
             15 PO-DRPA1321-PRORATASU  PIC S9(007)V9(02) PACKED-DECIMAL.
             15 PO-DRPA1321-PROVSATZ   PIC S9(003)V9(01) PACKED-DECIMAL.
             15 PO-DRPA1321-GESPROVBETR
                                       PIC S9(007)V9(02) PACKED-DECIMAL.
             15 PO-DRPA1321-EDKZ       PIC  X(001).
             15 PO-DRPA1321-DDRKZ      PIC  X(001).
             15 PO-DRPA1321-AUSKZ      PIC  X(001).
             15 PO-DRPA1321-FILLER2    PIC  X(007).
             15 PO-DRPA1321-STRUKANZ   PIC  9(002).
             15 PO-DRPA1321-STRUKTABELLE.
              20 PO-DRPA1321-STRUKTAB  OCCURS 2 TO 20
                          DEPENDING ON PO-DRPA1321-STRUKANZ.
               25 PO-DRPA1321-VBNR     PIC  9(007)       PACKED-DECIMAL.
               25 PO-DRPA1321-STRUKSTU PIC  9(003).
               25 PO-DRPA1321-DIFFPROV PIC S9(003)V9(01) PACKED-DECIMAL.
               25 PO-DRPA1321-PROVBETR PIC S9(005)V9(02) PACKED-DECIMAL.
               25 PO-DRPA1321-PROVRUECK
                                       PIC S9(005)V9(02) PACKED-DECIMAL.
               25 PO-DRPA1321-PROVEINH PIC S9(006)V9(01) PACKED-DECIMAL.
