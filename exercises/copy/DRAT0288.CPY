      *---------------------------------------------------------------
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------
      *
      *---------------------------------------------------------------
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------
      *    $Workfile:   drat0288.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drat0288.cpv  $
      *
      *   Rev 2.2   Apr 12 1999 12:02:22   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   Rev 2.0   FEB 27 1999 11:57:46   P13SP20
      *NT-UMSTELLUNG
      *
      *   Rev 1.0   13 JUL 1998 09:29:20   P13SP13
      *INITIAL REVISION.
      *
      ****************************************************************
      *    COPY-MEMBER: DRAT0288 FUNKTION EURO-SWITCH ERMITTELN      *
      *                          BETRAGSFELDER JE MASKE              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    19.03.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAT00005!19.03.99!    !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0288                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01  PI-DRAT0288.
           05  PI-DRAT0288-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
           05 PI-DRAT0288-AUSWAHL-KRITERIEN.
             10 PI-DRAT0288-ESBM-PGNAME         PIC  X(008).
             10 PI-DRAT0288-ESBM-BILDNR         PIC S9(004) BINARY.

      *
       01 PO-DRAT0288.
           05 PO-DRAT0288-RC                     PIC  X(003).
             88 PO-DRAT0288-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0288-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0288-RC-EOF                       VALUE 'EOF'.
           05 PO-DRAT0288-ESBM-DATEN.
             10 PO-DRAT0288-ESBM-POS            PIC S9(004) BINARY.
             10 PO-DRAT0288-ESBM-LAENGE         PIC S9(004) BINARY.
             10 PO-DRAT0288-ESBM-ANZ-DEZ        PIC S9(004) BINARY.
             10 PO-DRAT0288-ESBM-1000ER-PUNKT   PIC  X(001).
      *
      ******** Ende Copy-Member **************************************
