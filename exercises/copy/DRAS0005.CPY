      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAS0005.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRAS0005.CPv  $
      *
      *   Rev 2.3   Mar 16 2016 13:26:08   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.2   Mar 16 2016 12:49:00   t13ae30
      *AAS01158: Siegeradler Wettbewerb 2016
      *
      *   Rev 2.1   Dec 21 1999 09:14:40   T13AE27
      *neue Funktion
      *
      *   Rev 2.0   Feb 27 1999 11:26:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   12 Jan 1999 15:55:44   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRAS0005                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Service-Modul UPAS0002               *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Dürr                                             *
      *    DATUM:    28.07.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !28.07.99! 01 ! NEU                       ! Dürr     *
      * AAS01158!01.02.16! 02 ! NEUE FUNKTION DRAS0043    ! TH.WILLIG*
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
       01  PI-UPAS0002.
           05  PI-UPAS0002-FUNKTION            PIC  X(008).
               88 PI-UPAS0002-DRAS0006              VALUE 'DRAS0006'.
010216         88 PI-UPAS0002-DRAS0043              VALUE 'DRAS0043'.
      *
      ******* ENDE COPY-MEMBER **************************************



