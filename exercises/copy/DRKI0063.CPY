      * T13EX26   Stand: 31/07/98    14:14:21
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0063.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0063.CPv  $
      *
      *   Rev 2.4   Sep 11 2019 13:15:36   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.3   Mar 12 2019 15:03:20   t13ex44
      *APA02814: Familieninterne DL-Nachfolge-Regelung: Übertragung
      * Praxisbestand
      *
      *   Rev 2.2   Jul 17 2006 12:27:20   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.1   Jul 13 1999 17:11:10   T13AE34
      *AAN00190-Mappen DRKD0039 au KI
      *
      *   Rev 2.0   Apr 16 1999 18:10:18   T13EX48
      *AKI00028
      *Funktions-Konstanten ZJKI0001
      *
      *   Rev 1.0   29 Sep 1997 11:44:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0063  FUNKTION JOIN-OZP                  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    05.11.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !05.11.97! 01 ! NEUERSTELLUNG             ! Parent   *
      *         !        !    !                           !          *
      * apa00012!21.07.98! 1.1! History-Lesen Kundenübertr! Parent   *
      *         !        !    ! hinzugekommen             !          *
      *         !        !    !                           !          *
      * AKI00001!20.11.98! 1.2! FUNKTION DRKI0087         ! HSE      *
      *         !        !    ! EINGEFUEGT                !          *
      *         !        !    !                           !          *
      * AAN00190!30.06.99! 2.0! DRKI0880                  ! HEIDER   *
      *         !        !    !                           !          *
      * APA00409!14.03.06! 2.2! DRKI1871                  ! SCHEIDEL *
      *         !        !    ! DRKI1903                  !          *
      *         !        !    !                           !          *
      * APA02814!07.03.19! 2.3! DRKI3625                  ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
       01  PI-ZJKI0001.
           05 PI-ZJKI0001-FUNKTION            PIC X(008).
              88 PI-ZJKI0001-DRKI0064             VALUE 'DRKI0064'.
              88 PI-ZJKI0001-DRKI0087             VALUE 'DRKI0087'.
              88 PI-ZJKI0001-DRKI0385             VALUE 'DRKI0385'.
              88 PI-ZJKI0001-DRKI0880             VALUE 'DRKI0880'.
140306        88 PI-ZJKI0001-DRKI1871             VALUE 'DRKI1871'.
140306        88 PI-ZJKI0001-DRKI1903             VALUE 'DRKI1903'.
070319        88 PI-ZJKI0001-DRKI3625             VALUE 'DRKI3625'.


