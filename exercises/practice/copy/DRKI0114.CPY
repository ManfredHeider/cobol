      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0114.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0114.CPv  $
      *
      *   Rev 2.4   Jan 02 2008 17:30:26   t13ex48
      *AKL00666: Anpassung RUECK-DTA für Allfinanz PGM KUe
      *
      *   Rev 2.3   03 Dec 2007 11:28:48   t13ex39
      *AKL00664: U P K I 0 2 0 9
      *
      *   Rev 2.2   04 Oct 2007 13:35:42   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.1   04 Oct 2007 13:32:52   t13ex39
      *AKL00664: U P K I 0 1 6 6
      *
      *   Rev 2.0   Feb 27 1999 12:52:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:46:28   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   29 Sep 1997 11:48:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0114   LESE-OZP TABLE MITARBEITER        *
      *    -------------------------------------------------------   *
      *    ERSTELLT: JOST                                            *
      *    DATUM:    17.02.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00003!17.02.98! 01 ! NEUERSTELLUNG             ! S.JOST   *
      *         !        !    !                           !          *
      * AKI00001!15.07.98! 01 ! Erweiterung um die Fktn   ! Veit     *
      *         !        !    ! 'Lesen Mitarbeiter mit    !          *
      *         !        !    ! Mitarbeiternr'            !          *
      * --------!--------!----!---------------------------!----------*
      * AKL00644!30.08.07! 03 ! neues SELECT drki2069     ! WHO      *
      * --------!--------!----!---------------------------!----------*
191207* AKL00666!19.12.07! 03 ! Neues SEL mit PERS-ID:    ! HSE      *
      *         !        !    ! zus. Ermittlung Mandant   !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  PI-ZWKI0068.
           05 PI-ZWKI0068-FUNKTION            PIC X(008).
              88 PI-ZWKI0068-DRKI0115             VALUE 'DRKI0115'.
              88 PI-ZWKI0068-DRKI0222-SEL         VALUE 'DRKI0222'.
              88 PI-ZWKI0068-DRKI2069             VALUE 'DRKI2069'.
191207        88 PI-ZWKI0068-DRKI2221             VALUE 'DRKI2221'.

