      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0145.CPY  $
 
 
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRVB0145.CPv  $
      *
      *   Rev 2.8   Mar 11 2016 10:53:14   t13ex71
      *ARW00109: Anpassungen im PMRW0017 (BPR)
      *
      *   Rev 2.7   31 Oct 2007 14:28:54   t13ae30
      *AVB00905: Allfinanz - Bausteine, Stammdaten-DTA TWI
      *
      *   Rev 2.6   May 30 2003 10:45:32   t13ae35
      *ACV00016 - neue Funktion DRVB0660
      *
      *   Rev 2.5   Apr 30 2002 09:09:20   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.4   Oct 21 1999 14:53:42   T13EX50
      *AKI0003 - Produktionsübergabe :
      *                Neues Bestellverfahren
      *                "Der Vermögensberater"
      *
      *   Rev 2.3   Jul 01 1999 10:30:00   T13AE42
      *AKI00003 - Check Bestellberechtigung
      *
      *   Rev 2.2   May 10 1999 10:22:00   T13AE30
      *AAS00051-Stammdaten-DTA ---> Aachen
      *
      *   Rev 2.1   Apr 27 1999 17:00:02   T13AE42
      *
      *
      *   Rev 2.0   Feb 27 1999 15:58:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   13 Aug 1998 16:47:46   T13AE35
      *AAS00049 - Aenderung 2 neue Funktionen
      *
      *   Rev 1.0   Feb 19 1997 09:58:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0145                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049  MAT                   *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: PARENT                                           *
      *    DATUM:    25.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !25.07.97! 01 ! NEU                       ! PARENT   *
      * AAS00049!20.06.98! 02 ! NEUE FUNKTION DRVB0282    ! WALB     *
      * AKI00001!15.07.98! 03 ! NEUE FUNKTION DRVB0261    !Zgur-Vujic*
      * AAS00127!18.03.99! 04 ! NEUE FUNKTION DRVB0369    ! WALB     *
      * AAS00051!07.05.99! 05 ! NEUE FUNKTION DRVB0389    ! WILLIG   *
      * AKI00003!01.07.99! 06 ! NEUE FUNKTION DRVB0244    ! SCHMIDT  *
      * ACV00007!17.10.01! 07 ! NEUE FUNKTION DRVB0559,   ! P.DÜRR   *
      *         !        !    !               DRVB0578,   !          *
      *         !        !    !               DRVB0579    !          *
      * ACV00016!22.05.03! 2.6! NEUE FUNKTION DRVB0660    ! J.Hering *
      * AVB00905!29.06.07! 2.7! NEUE FUNKTION DRVB0858    !TH.WILLIG *
      * ARW00109!08.03.16! 2.8! NEUE FUNKTION DRVB1129    !B.Pretsch *
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
       01  PI-ZWVB0049.
           05 PI-ZWVB0049-FUNKTION            PIC X(008).
              88 PI-ZWVB0049-DRVB0146             VALUE 'DRVB0146'.
200698        88 PI-ZWVB0049-DRVB0282             VALUE 'DRVB0282'.
150798        88 PI-ZWVB0049-DRVB0261             VALUE 'DRVB0261'.
180399        88 PI-ZWVB0049-DRVB0369             VALUE 'DRVB0369'.
070599        88 PI-ZWVB0049-DRVB0389             VALUE 'DRVB0389'.
010799        88 PI-ZWVB0049-DRVB0244             VALUE 'DRVB0244'.
171001        88 PI-ZWVB0049-DRVB0559             VALUE 'DRVB0559'.
130302        88 PI-ZWVB0049-DRVB0578             VALUE 'DRVB0578'.
130302        88 PI-ZWVB0049-DRVB0579             VALUE 'DRVB0579'.
220503        88 PI-ZWVB0049-DRVB0660             VALUE 'DRVB0660'.
180607        88 PI-ZWVB0049-DRVB0858             VALUE 'DRVB0858'.
080316        88 PI-ZWVB0049-DRVB1129             VALUE 'DRVB1129'.

