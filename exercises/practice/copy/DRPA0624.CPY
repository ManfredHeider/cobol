      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0624.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0624.CPv  $
      *
      *   Rev 2.1   Dec 20 2013 10:13:36   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.0   May 27 1999 13:06:56   T13EX25
      *APA00042, Neuerstellung V2.0
      *
      *   Rev 1.0   11 Nov 1997 16:55:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0624                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXPA0111                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Jürgen Jäger-Sokoluk                             *
      *    DATUM:    12.11.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!12.11.97!    ! NEU                       ! JäGER    *
      * AFPA0012!30.12.97!    ! FUNKTION DRPA0693         ! MUND     *
      * APA00012!30.06.98!    ! FUNKTION DRPA0693         ! STORCH   *
      * APA00012!21.07.98!    ! FUNKTION DRPA1023         ! D.WILHELM*
      * APA01526!06.08.13!2.1 ! DRPA1754 HT-PROD-SCHL ERM.! BUCH     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL
      *    PRO UPRO ÜBERGEBEN WERDEN.
      *    ALSO NICHT: DATEN, DIE PRO FUNKTION ÜBERGEBEN WERDEN
      *                (IN SEPARATEN COPYS PRO FUNKTION)
      *
      ****************************************************************
      *
       01  PI-ZXPA0111.
           05  PI-ZXPA0111-FUNKTION            PIC  X(008).
               88 PI-ZXPA0111-DRPA0625              VALUE 'DRPA0625'.
301297         88 PI-ZXPA0111-DRPA0693              VALUE 'DRPA0693'.
300698         88 PI-ZXPA0111-DRPA0978              VALUE 'DRPA0978'.
210798         88 PI-ZXPA0111-DRPA1023              VALUE 'DRPA1023'.
230413         88 PI-ZXPA0111-DRPA1754              VALUE 'DRPA1754'.
      *
      ******* ENDE COPY-MEMBER **************************************
