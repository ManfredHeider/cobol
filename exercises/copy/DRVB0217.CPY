      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0217.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRVB0217.CPv  $
      *
      *   Rev 2.3   Oct 29 1999 17:40:14   T13AE42
      *AOP00177 - Neue Funktion DRVB0247
      *
      *   Rev 2.2   Jun 30 1999 16:49:18   T13AE28
      *AAT00011: Drei neue Funktionen für KI
      *
      *   Rev 2.1   Jun 10 1999 12:05:26   T13AE28
      *AAT00011: Impl. von zwei neuen Fkt. mit Grenze 50 und 05
      *
      *   Rev 2.0   Feb 27 1999 15:58:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   17 Dec 1998 11:36:24   T13AE28
      *AOP00161 - Neue Fkt. DRVB0341 u. DRVB0343
      *
      *   Rev 1.2   03 Sep 1998 11:53:48   T13AE28
      *AVB00426 - Funktionen UPVB0104
      *
      *   Rev 1.1   17 Jul 1998 16:06:02   T13AE28
      *AAT00004 - Funktion fuer Window (UPVB0104)
      *
      *   Rev 1.0   23 Jul 1997 12:17:20   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0217                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO UPVB0104                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                        *
      *    DATUM:    20.03.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !20.03.98! 01 ! NEU                       ! D.Ehrmann*
      *         !        !    !                           !          *
      * AVB00432!11.11.98! 02 ! NEUE FUNKTION DRVB0341    ! D.EHRMANN*
      *         !        !    ! FüR KOMPL.KEY IN OPAL     !          *
      * AOP00177!29.10.99! 03 ! NEUE FUNKTION DRVB0247    ! B.SCHMIDT*
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
       01  PI-UPVB0104.
           05  PI-UPVB0104-FUNKTION            PIC  X(008).
               88 PI-UPVB0104-DRVB0218              VALUE 'DRVB0218'.
               88 PI-UPVB0104-DRVB0220              VALUE 'DRVB0220'.
               88 PI-UPVB0104-DRVB0221              VALUE 'DRVB0221'.
               88 PI-UPVB0104-DRVB0222              VALUE 'DRVB0222'.
               88 PI-UPVB0104-DRVB0223              VALUE 'DRVB0223'.
               88 PI-UPVB0104-DRVB0224              VALUE 'DRVB0224'.
               88 PI-UPVB0104-DRVB0225              VALUE 'DRVB0225'.
               88 PI-UPVB0104-DRVB0226              VALUE 'DRVB0226'.
               88 PI-UPVB0104-DRVB0227              VALUE 'DRVB0227'.
               88 PI-UPVB0104-DRVB0228              VALUE 'DRVB0228'.
               88 PI-UPVB0104-DRVB0229              VALUE 'DRVB0229'.
               88 PI-UPVB0104-DRVB0232              VALUE 'DRVB0232'.
               88 PI-UPVB0104-DRVB0233              VALUE 'DRVB0233'.
               88 PI-UPVB0104-DRVB0235              VALUE 'DRVB0235'.
               88 PI-UPVB0104-DRVB0236              VALUE 'DRVB0236'.
               88 PI-UPVB0104-DRVB0241              VALUE 'DRVB0241'.
               88 PI-UPVB0104-DRVB0242              VALUE 'DRVB0242'.
               88 PI-UPVB0104-DRVB0243              VALUE 'DRVB0243'.
291099         88 PI-UPVB0104-DRVB0247              VALUE 'DRVB0247'.
               88 PI-UPVB0104-DRVB0284              VALUE 'DRVB0284'.
               88 PI-UPVB0104-DRVB0286              VALUE 'DRVB0286'.
               88 PI-UPVB0104-DRVB0288              VALUE 'DRVB0288'.
               88 PI-UPVB0104-DRVB0290              VALUE 'DRVB0290'.
               88 PI-UPVB0104-DRVB0302              VALUE 'DRVB0302'.
               88 PI-UPVB0104-DRVB0303              VALUE 'DRVB0303'.
               88 PI-UPVB0104-DRVB0314              VALUE 'DRVB0314'.
               88 PI-UPVB0104-DRVB0315              VALUE 'DRVB0315'.
               88 PI-UPVB0104-DRVB0341              VALUE 'DRVB0341'.
               88 PI-UPVB0104-DRVB0343              VALUE 'DRVB0343'.
               88 PI-UPVB0104-DRVB0416              VALUE 'DRVB0416'.
               88 PI-UPVB0104-DRVB0417              VALUE 'DRVB0417'.
               88 PI-UPVB0104-DRVB0418              VALUE 'DRVB0418'.
      *
      ******* ENDE COPY-MEMBER **************************************
