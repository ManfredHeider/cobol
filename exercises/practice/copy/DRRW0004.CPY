      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRRW0004.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRRW0004.CPv  $
      *
      *   Rev 2.1   27 Sep 2000 09:59:24   T13AE27
      *Anpassung AVBG
      *
      *   Rev 2.0   Feb 27 1999 14:51:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   23 Sep 1998 12:27:34   T13AE35
      *AIS00009 - Neuerstellung, ALLIS FIN/CO-spezif. Konstanten
      *
      *   Rev 1.0   10 Mar 1998 17:18:42   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *                                                               *
      *            COPY-MEMBER  ALLIS FIN/CO spezifische Konstanten   *
      *            COPYMEMBER:  DRRW0004                              *
      *                                                               *
      *---------------------------------------------------------------*
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00030!19.03.98!    ! NEU                       ! Moll     *
      *         !        !    !                           !          *
      * ASL00060!04.09.00!2.0 ! Anpassungen wegen AVBG    ! P.Dürr   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************

       01  FILLER.
           05  K-AFC-STATUS-NEU         PIC  9(001)   VALUE 1.
           05  K-AFC-STATUS-AEND        PIC  9(001)   VALUE 2.
           05  K-AFC-STATUS-LOESCH      PIC  9(001)   VALUE 9.

           05  K-AFC-KONTOROLLE-SACH    PIC  X(003)   VALUE 'SK '.

           05  K-AFC-HABEN              PIC X(001)    VALUE '0'.
           05  K-AFC-SOLL               PIC X(001)    VALUE '1'.

           05  K-AFC-WAEHRUNG-DEM       PIC X(03)     VALUE 'DEM'.
040900     05  K-AFC-WAEHRUNG-ATS       PIC X(03)     VALUE 'ATS'.
040900     05  K-AFC-WAEHRUNG-EUR       PIC X(03)     VALUE 'EUR'.

           05  K-AFC-AUSSENSTELLE-DUMMY PIC X(02)     VALUE '00'.
           05  K-AFC-STOP-DUMMY         PIC X(01)     VALUE '0'.

      *        * Dummy-Geburtsdatum für Sonderkonten, EDs,...
           05  K-AFC-GEBDATUM-DUMMY     PIC X(008)    VALUE '01011900'.

           05  K-AFC-BV-HAUPT-JA        PIC X(001)    VALUE '1'.
           05  K-AFC-BV-HAUPT-NEIN      PIC X(001)    VALUE '0'.

      *        * Bankverbindung 1 / Standard-Bankverbindung
           05  K-AFC-BV-NR-1            PIC X(001)    VALUE '1'.
      *        * Bankverbindung 2 / OLZ-Bankverbindung
           05  K-AFC-BV-NR-2            PIC X(001)    VALUE '2'.
      *        * Gültigbis-Datum für ungültige Bakverbindungen
           05  K-AFC-BV-DAT-BIS-UNGUELTIG  PIC X(008) VALUE '01011997'.

