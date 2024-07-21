      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0694.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\DRKI0694.CPv  $
      *
      *   Rev 2.2   01 Mar 2001 13:56:38   T13EX39
      *FKI00454 Komm. bei Haushalt
      *
      *   Rev 2.1   26 Sep 2000 17:17:04   T13EX12
      *AKI00141 - Neue Kundensuche
      *
      *   Rev 2.0   Feb 27 1999 12:58:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 19:11:02   T13EX47
      *Funktionen des OZP ZXKI0043
      *
      *   Rev 1.0   25 Aug 1998 09:31:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0694                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXAT0005                        *
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
      * -------------------------------------------------------------*
      * AKI00115!31.08.00! 02 ! Neue Funktion für Lesen   !v.d.Goltz *
      *         !        !    ! Gebdat mit Attribut Null. !          *
      *         !        !    !                           !          *
      * -------------------------------------------------------------*
      * FKI00454!21.02.01! 03 ! zus. Select für alle Haus-!WHO       *
      *         !        !    ! halte                     !          *
      *         !        !    !                           !          *
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
       01  PI-ZXKI0043.
           05  PI-ZXKI0043-FUNKTION            PIC  X(008).
               88 PI-ZXKI0043-DRKI0695              VALUE 'DRKI0695'.
               88 PI-ZXKI0043-DRKI0701              VALUE 'DRKI0701'.
310800         88 PI-ZXKI0043-DRKI1122              VALUE 'DRKI1122'.
210201         88 PI-ZXKI0043-DRKI1192              VALUE 'DRKI1192'.
      *
      ******* ENDE COPY-MEMBER DRKI0694 *****************************
