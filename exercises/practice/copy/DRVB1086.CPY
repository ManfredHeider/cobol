      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1086.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB1086.CPv  $
      *
      *   Rev 2.4   May 21 2015 13:18:04   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.3   May 18 2015 16:17:26   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.2   Apr 28 2015 12:57:58   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.1   Mar 13 2015 10:00:36   t13ae35
      *AVB01347: Lese-OZP/Schreibmodul für USTAT
      *
      *   Rev 2.0   Mar 13 2015 08:58:50   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1086                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0125                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.HERING                                         *
      *    DATUM:    27.04.2015                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01347!27.04.15!2.1 ! NEU                       ! J.HERING  *
AN1198* AVB01347!28.04.15!2.2 ! FUNKT. DRVB1089 für Antrag! J.Hering  *
180515* AVB01347!18.04.15!2.3 ! FUNKT. DRVB1098 für Daisy ! J.Hering  *
210515* AVB01347!21.05.15!2.4 ! FUNKT. DRVB1101 für VB39  ! J.Hering  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO UPRO ÜBERGEBEN WERDEN --> FUNKTIONEN-COPY              *
      *                                                               *
      *****************************************************************
 
       01  PI-ZWVB0125.
           05  PI-ZWVB0125-FUNKTION            PIC  X(008).
               88 PI-ZWVB0125-DRVB1087              VALUE 'DRVB1087'.
AN1198         88 PI-ZWVB0125-DRVB1089              VALUE 'DRVB1089'.
180515         88 PI-ZWVB0125-DRVB1098              VALUE 'DRVB1098'.
210515         88 PI-ZWVB0125-DRVB1101              VALUE 'DRVB1101'.
 
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1086   --------------------------*
      *---------------------------------------------------------------*
 
 
 

