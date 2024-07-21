      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0268.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRAT0268.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:44   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 17:58:16   T13EX26
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   01 Apr 1998 08:39:02   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAT0268 FUNKTION ERMITTELN EINER NOTIZ      *
      *                                   ZU EINER ART VON OBJEKT    *
      *    -------------------------------------------------------   *
      *    ERSTELLT: FOLTZ                                           *
      *    DATUM:    23.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!23.06.98! 01 !NEU                        !FOLTZ     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAT0268                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    FUNKTION: ERMITTELN EINER NOTIZ ZU EINER ART VON OBJEKT   *
      *              BEREITSTELLEN CONTAINERNOTIZ                    *
      ****************************************************************
       01 PI-DRAT0268.
           05  PI-DRAT0268-UFKT                   PIC     99.
               88 PI-DRAT0268-GET-ALL             VALUE   01.
               88 PI-DRAT0268-GET-NEXT            VALUE   03.
      *
      *   KANN BELEGT WERDEN MIT 01=GET ALL, 03=GET-NEXT,
      *   KANN  *NICHT*  BELEGT WERDEN MIT 02=GET FIRST, GET QUIT
      *
       01 PO-DRAT0268.
          05 PO-DRAT0268-RC                        PIC  X(003).
             88 PO-DRAT0268-RC-OK                        VALUE 'OK '.
             88 PO-DRAT0268-RC-LEER                      VALUE 'LER'.
             88 PO-DRAT0268-RC-EOF                       VALUE 'EOF'.
          05 PO-DRAT0268-ERGEBNISBLOCK.
             10 PO-DRAT0268-NTZZ-DATEN.
                15 PO-DRAT0268-NTZZ-ANZ            PIC  S9(04) BINARY.
                15 PO-DRAT0268-NTZZ-TAB.
                   20 PO-DRAT0268-NTZZ-TABELLE   OCCURS 10
                                                 INDEXED DRAT0268-IX1.
                      25 PO-DRAT0268-NTZZ-ZEILE    PIC  X(080).
      *
      ******** Ende Copy-Member **************************************
