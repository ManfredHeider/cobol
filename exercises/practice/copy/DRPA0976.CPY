      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0976.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0976.CPv  $
      *
      *   Rev 2.1   Jul 03 2020 07:09:42   t13ex67
      *FPA01775: Abend beim Aufruf einer EVS-Notiz 
      *
      *   Rev 2.0   May 27 1999 11:42:46   T01JM2S
      *NT-Umstellung
      *
      *   Rev 1.0   01 Apr 1998 14:19:56   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0976                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPPA0103                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    22.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!22.06.98! 01 ! NEU                       ! WOLFF    *
      * AFPA0012!27.08.98!    ! NEU: DRPA1049             ! WOLFF    *
      * FPA01775!01.07.20!2.01! NEU: DRPA1946             ! STORCH   *
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
       01  PI-UPPA0103.
           05  PI-UPPA0103-FUNKTION            PIC  X(008).
               88 PI-UPPA0103-DRPA0977              VALUE 'DRPA0977'.
               88 PI-UPPA0103-DRPA1049              VALUE 'DRPA1049'.
               88 PI-UPPA0103-DRPA1946              VALUE 'DRPA1946'.
      *
      ******* ENDE COPY-MEMBER **************************************

