      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0989.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0989.CPv  $
      *
      *   Rev 2.2   Jul 16 2015 15:08:34   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.1   25 Apr 2000 10:14:24   T01JMBQ
      *APA00084 - Redesign SST-MA
      *
      *   Rev 2.0   May 27 1999 11:33:48   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   01 Apr 1998 14:21:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0989                                      *
      *    --------------------------------------------------------   *
      *    PAMS-DATEN                                                 *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ERB                                              *
      *    DATUM:    02.07.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!02.07.98!1.1 ! NEU                       ! ERB      *
      *         !        !    !                           !          *
      * APA00012!15.07.98!1.2 ! PEST-PEBD-DATEN in        ! Jäger    *
      *         !        !    ! PE-STRUKTUR-DATEN umbenamt!          *
      *         !        !    !                           !          *
      * APA00012!27.08.98!1.3 ! Neues Attribut eingefügt  ! Jäger    *
      *         !        !    !                           !          *
      * APA00012!02.09.98!1.4 ! Länge VARCHAR gekürzt     ! Jäger    *
      *         !        !    !                           !          *
      * AFPA0012!28.10.98!1.5 ! NEU: PDTA-ID              ! WOLFF    *
      *         !        !    !                           !          *
      * APA00084!14.03.00!2.1 ! KOMPLETTE ÜBERARBEITUNG   ! KRUFF    *
      *         !        !    ! - PAMS-BEWEGUNGSDATEN IN  !          *
      *         !        !    !   COPY-STRECKE DRPA1347   !          *
      *         !        !    ! - REDEFINITION            !          *
      *         !        !    !   PE-STRUKTUR-DATEN       !          *
      *         !        !    ! - KOMPLETTE ÄNDERUNG DER  !          *
      *         !        !    !   STUFENNUMMERN           !          *
      * APA01932!29.06.15!2.2 ! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *                                                              *
      ****************************************************************
           25 PAMS-DATEN.
              27 FILLER.
                 COPY DRPA1347.
              27 PE-STRUKTUR-DATEN.
                 30 PE-STRUKTUR-DATEN-LEN           PIC S9(004) BINARY.
290615*          30 PE-STRUKTUR-DATEN-TEXT          PIC  X(800).
290615           30 PE-STRUKTUR-DATEN-TEXT          PIC  X(1600).
              27 FILLER REDEFINES PE-STRUKTUR-DATEN.
                 COPY DRPA1031.
      *
      ******* ENDE COPY-MEMBER **************************************

