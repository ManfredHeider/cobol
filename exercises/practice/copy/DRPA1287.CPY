      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1287.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA1287.CPv  $
      *
      *   Rev 2.3   15 Jul 1999 15:19:40   T01JM2E
      *APA00012 - Binär in ungepackt geändert
      *
      *   Rev 2.2   13 Jul 1999 11:11:52   T01JM2E
      *APA00012 - BUCH-DATUM eingefügt
      *
      *   Rev 2.1   07 Jul 1999 09:35:20   T01JM2E
      *APA00012 - Neuerstellung
      *
      *   Rev 2.0   Mar 30 1999 08:44:24   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 10:18:54   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1287                                      *
      *    --------------------------------------------------------   *
      *    AUSGABEDATEI RÜCKMELDESÄTZE                                *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                        *
      *    DATUM:    12.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.07.99! 01 ! NEU                       !D.WILHELM *
      * APA00012!13.07.99! 02 ! BUCH-DATUM EINGEFÜGT      !D.WILHELM *          
      * APA00012!15.07.99! 2.2! BINÄR IN UNGEPACKT GEÄNDER!D.WILHELM *          
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DEN SATZAUFBAU FÜR DIE AUSGABE        *
      *    AUS DEM PROGRAMM PMPA0052 - RUECKMELDESÄTZE STATUS 40     *
      ****************************************************************
            05 PGEB-GES               PIC 99.                                   
            05 PVER-VNR               PIC  X(15).
            05 PVER-KUNDE-NAME        PIC  X(25).
            05 PROD-PROD-SCHL         PIC 99.                                   
            05 PDPT-PRODART           PIC  9.                                   
            05 PMPT-PPFL-BEITRAG      PIC S9(09)V9(2) USAGE COMP-3.
            05 PEST-AVERM             PIC  9(07).                               
            05 PMPT-EINH              PIC S9(06)V9(2) USAGE COMP-3.
            05 BUCH-DATUM             PIC  9(08).
