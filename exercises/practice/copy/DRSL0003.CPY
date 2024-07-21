      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRSL0003.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRSL0003.CPv  $
      *
      *   Rev 2.2   Mar 17 2000 12:06:30   T13AE35
      *ASL00047 - Dumy-Satz fuer Korr-Datei
      *
      *   Rev 2.1   Mar 16 2000 16:44:00   T13AE35
      *ASL00047 - Neu, Copy fuer SL000500
      *ORG-Korrekturdatei
      *
      *   Rev 2.0   Feb 11 2000 15:19:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00047!22.02.00!01.0! Neuerstellung             ! Walb     *
      * ASL00047!17.03.00!01.1! Dummy-Satz einfügen       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                               *
      *            COPY-MEMBER für Korrektur-Datei/ORG-Zuschuss       *
      *            DATEINUMMER: SL000500 QSAM                         *
      *            COPYMEMBER:  DRSL0003                              *
      *            SATZLAENGE:  68 BYTES                              *
      *                                                               *
      *---------------------------------------------------------------*
       01  DRSL0003.
           05  DRSL0003-REC.
               10  DIR-NEU           PIC  9(03).
               10  VBNR              PIC  9(07).
               10  VB-NAME           PIC  X(30).
               10  PF-KZ             PIC  X.
               10  DIR-ALT           PIC  9(03).
               10  KUEND-DAT         PIC  X(08).
               10  VBS-ALT           PIC  X(04).
               10  VBS-NEU           PIC  X(04).
               10  VBS-DAT           PIC  X(08).
           05  DRSL0003-DUMMY REDEFINES DRSL0003-REC.
               10  DUMMY-DIR-NEU     PIC  9(03).
               10  DUMMY-VBNR        PIC  9(07).
               10  DUMMY-QUARTAL     PIC  9(02).
               10  DUMMY-ERM-MM      PIC  9(02).
               10  DUMMY-ERM-JJ      PIC  9(04).
               10  FILLER            PIC  X(50).
      *---------------------------------------------------------------*
