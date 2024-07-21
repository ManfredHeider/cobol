      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM
      *            DATUM      :  27.10.2008
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2008 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1678.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1678.CPv  $
      *
      *   Rev 2.1   Nov 09 2009 10:19:00   t13ex44
      *APA00820: Pflegedialog Rückstellung/Haftzeiten
      *
      *   REV 2.0   OCT 27 2008 13:06:46   I13AZ23
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1678 LIST ZXPA0007                       *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: OBER                                            *
      *    DATUM:    27.10.2008                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00820!27.10.08!2.1 !NEU                        !OBER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-DRPA1678.
           05  PI-DRPA1678-UFKT                PIC  9(02).
           05  PI-DRPA1678-NAME-ZW-SPEICHER    PIC  X(08).
           05  PI-DRPA1678-AUSWAHL-KRITERIEN.
               10 FILLER                       PIC  X(01).
               10 PI-DRPA1678-PROD-PROD-SCHL   PIC  S9(4) USAGE COMP.
               10 PI-DRPA1678-PGEB-GES         PIC  S9(4) USAGE COMP.

      *
       01  PO-DRPA1678.
           05  PO-DRPA1678-RC                  PIC  X(03).
               88 PO-DRPA1678-RC-OK                        VALUE 'OK '.
               88 PO-DRPA1678-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA1678-RC-LEER                      VALUE 'LER'.
           05  PO-DRPA1678-ANZAHL-ERG-ZEILEN   PIC  9(05).

           05  PO-DRPA1678-ERGEBNISZEILE.
               10 PO-DRPA1678-PRCK-SELKRIT       PIC  X(15).
               10 PO-DRPA1678-PRCK-GUELT-AB      PIC  X(10).
               10 PO-DRPA1678-PRCK-GUELT-BIS     PIC  X(10).
               10 PO-DRPA1678-KTY-HAFTZEITTYP    PIC  S9(4) USAGE COMP.
               10 PO-DRPA1678-KNR-HAFTZEITTYP    PIC  S9(4) USAGE COMP.
               10 PO-DRPA1678-PRCK-HAFTZT-PG     PIC  S9(4) BINARY.
               10 PO-DRPA1678-PRCK-HAFTZT-VB     PIC  S9(4) BINARY.
               10 PO-DRPA1678-PRCK-LAST-UPDATE   PIC  X(26).
               10 PO-DRPA1678-PRCK-KENNUNG       PIC  X(10).
      *
      ******* ENDE COPY-MEMBER **************************************


      *---------------------------------------------------------------*
