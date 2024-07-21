      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0706.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/MF/CPY/DRKI0706.CPv  $
      *
      *   Rev 2.5   Oct 13 2003 11:46:46   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.4   Jan 20 2003 09:45:12   t13ex27
      *AKI00303 - Kommissarische Kundenübertragung
      *
      *   Rev 2.3   Jun 13 2002 08:01:00   t13ex49
      *AKI00274 - neue Funktion
      *
      *   Rev 2.2   31 Jan 2001 14:25:04   T13EX39
      *AKI00068 KBI
      *
      *   Rev 2.1   06 Jul 2000 13:27:10   T13EX39
      *AKI00100 BFG-INVEST
      *
      *   Rev 1.1   08 Feb 1999 18:19:30   T13EX47
      *Funktionsauswahl fuer LIST-OZP ZXKI0359
      *
      *   Rev 1.0   06 Oct 1998 15:31:08   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0706                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXKI0359                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    13.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKIB0001!13.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *                                                              *
      * -------------------------------------------------------------*
      * AKI00068!14.03.00! 02 ! NEUES SELECT FÜR DRKI0962 ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00100!14.03.00! 02 ! NEUES SELECT              ! WHO      *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * AKI00259!13.12.01! 02 ! NEUER SELECT              ! P. Pohl  *
      * -------------------------------------------------------------*
230402* AKI00274!23.04.02!    ! NEUER SELECT DRKI1411     ! P. Pohl  *
      * -------------------------------------------------------------*
      * AKI00303!27.09.02!2.4 ! Funktion DRKI1449 eingef. !I.Frenzel *
      *         !        !    ! für KOKÜ                  !          *
      * -------------------------------------------------------------*
040903* AKI00114!04.09.03!2.5 ! Funktion DRKI1577 eingef. ! P. Pohl  *
040903*         !        !    ! (alle Verträge zu einer   !          *
040903*         !        !    ! ADRESSEID)                !          *
      *         !        !    ! und DRKI1577 (alle        !          *
      *         !        !    ! Verträge zu einer Person  !          *
      *         !        !    ! and einer Adresse)        !          *
      *         !        !    !                           !          *
      *                                                              *
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
       01  PI-ZXKI0359.
           05  PI-ZXKI0359-FUNKTION            PIC  X(008).
               88 PI-ZXKI0359-DRKI0707              VALUE 'DRKI0707'.
               88 PI-ZXKI0359-DRKI0708              VALUE 'DRKI0708'.
               88 PI-ZXKI0359-DRKI0962              VALUE 'DRKI0962'.
               88 PI-ZXKI0359-DRKI1098              VALUE 'DRKI1098'.
131201         88 PI-ZXKI0359-DRKI1311              VALUE 'DRKI1311'.
230402         88 PI-ZXKI0359-DRKI1411              VALUE 'DRKI1411'.
270902         88 PI-ZXKI0359-DRKI1449              VALUE 'DRKI1449'.
040903         88 PI-ZXKI0359-DRKI1576              VALUE 'DRKI1576'.
040903         88 PI-ZXKI0359-DRKI1577              VALUE 'DRKI1577'.
      *
      ******* ENDE COPY-MEMBER DRKI0706 *****************************
