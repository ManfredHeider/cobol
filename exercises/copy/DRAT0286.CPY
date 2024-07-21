      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   drat0286.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drat0286.cpv  $
      *
      *   Rev 2.2   Apr 12 1999 12:02:20   T13AE34
      *AAT00005-Euro-Switch (hier: PVCS-Kopf)
      *
      *   Rev 2.0   FEB 27 1999 11:57:46   P13SP20
      *NT-UMSTELLUNG
      *
      *   Rev 1.0   13 JUL 1998 09:29:12   P13SP13
      *INITIAL REVISION.
      *
      *---------------------------------------------------------------*

      ****************************************************************
      *    COPY-MEMBER: DRAT0286 FUNKTION EURO-SWITCH ERMITTELN      *
      *                          WäHRUNGEN JE BEZUGSGROESSE          *
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
      *    FÜR DIE FUNKTION DRAT0286                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0286.
           05  PI-DRAT0286-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0286-AUSWAHL-KRITERIEN.
             10 PI-DRAT0286-ESWB-PGNAME         PIC  X(008).
             10 PI-DRAT0286-ESWB-BILDNR         PIC S9(004) BINARY.

      *
       01 PO-DRAT0286.
          05 PO-DRAT0286-RC                     PIC  X(003).
             88 PO-DRAT0286-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0286-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0286-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0286-ESWB-DATEN.
             10 PO-DRAT0286-ESWB-BEZGROESSE     PIC  X(015).
             10 PO-DRAT0286-ESWB-POS            PIC S9(004) BINARY.
             10 PO-DRAT0286-ESWB-LAENGE         PIC S9(004) BINARY.
             10 PO-DRAT0286-ESWB-STD-WAE        PIC S9(004) BINARY.
             10 PO-DRAT0286-ESWB-ALT-WAE        PIC S9(004) BINARY.
             10 PO-DRAT0286-ESWB-SUCHFOLGE      PIC S9(004) BINARY.
      *
      ******** Ende Copy-Member **************************************
