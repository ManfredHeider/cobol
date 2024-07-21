      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0407.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/MF/CPY/DRVB0407.CPv  $
      *
      *   Rev 2.3   Jul 24 2002 10:24:54   t13ae35
      *FSL00099 - neue Funktion DRVB0593 RFT zum Zeitpunkt
      *
      *   Rev 2.2   20 Sep 2000 15:48:06   T13AE27
      *DIM7-AVBG
      *
      *   Rev 2.1   Jun 04 1999 12:45:40   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 27 1999 13:56:22   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0407                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0005  RFT                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    31.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!31.05.99! 01 ! NEU                       !SCHMIDT,B.*
040900* ASL00060!04.09.00! 02 ! Anpassungen wegen AVBG    ! P.Dürr   *
230702* FSL00099!23.07.02! 03 ! alle RFT's zum Zeitpunkt  ! J.Hering *
      * DSY04437!27.04.22!    ! NEUE FUNKTION DRVB1343    ! U131764  *
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
       01  PI-ZXVB0005.
           05 PI-ZXVB0005-FUNKTION            PIC X(008).
              88 PI-ZXVB0005-DRVB0408             VALUE 'DRVB0408'.
040900        88 PI-ZXVB0005-DRVB0501             VALUE 'DRVB0501'.
230702        88 PI-ZXVB0005-DRVB0593             VALUE 'DRVB0593'.
              88 PI-ZXVB0005-DRVB1343             VALUE 'DRVB1343'.

