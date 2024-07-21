      *---------------------------------------------------------------*
      *    STANDARD FÜR ALLE OBJEKTE UNTER PVCS
      *            DATUM   :  NOVEMBER 1995
      *            KOPF AUS:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    COPYRIGHT 1995 ATLAS DIENSTLEISTUNGS GMBH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0290.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\DRAT0290.CPv  $
      *
      *   Rev 2.4   Jun 29 1999 10:25:30   T13AE34
      *FRZ00028-PVCS Korrektur
      *
      *   Rev 2.3   JUN 29 1999 09:57:16   T13AE34
      *FRZ00028-PVCS Korrektur
      *
      *   Rev 2.2   APR 12 1999 09:57:16   T13AE34
      *AAT00005-Euro-Switch
      *
      *   Rev 2.2   APR 12 1999 09:57:16   T13AE34
      *PVCS
      *
      *   Rev 2.1   APR 06 1999 09:57:16   T13AE34
      *AAT00005-Euro-Switch
      *
      *   Rev 2.0   MAR 16 1999 09:57:16   P13SP20
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0290 FUNKTION EURO-SWITCH ERMITTELN      *
      *                          WäHRUNGSTEXTE JE MASKE              *
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
      *    FÜR DIE FUNKTION DRAT0290                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAT0290.
           05  PI-DRAT0290-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST-NEXT, 3=QUIT
          05 PI-DRAT0290-AUSWAHL-KRITERIEN.
             10 PI-DRAT0290-ESWT-PGNAME         PIC  X(008).
             10 PI-DRAT0290-ESWT-BILDNR         PIC S9(004) BINARY.

      *
       01 PO-DRAT0290.
          05 PO-DRAT0290-RC                     PIC  X(003).
             88 PO-DRAT0290-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0290-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0290-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0290-ESWT-DATEN.
             10 PO-DRAT0290-ESWT-POS            PIC S9(004) BINARY.
             10 PO-DRAT0290-ESWT-LAENGE         PIC S9(004) BINARY.
             10 PO-DRAT0290-ESWT-STD-WTEXT      PIC  X(020).
             10 PO-DRAT0290-ESWT-ALT-WTEXT      PIC  X(020).
      *
      ******** Ende Copy-Member **************************************
