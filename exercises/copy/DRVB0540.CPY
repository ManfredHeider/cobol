      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0540.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0540.CPv  $
      *
      *   Rev 2.4   Mar 14 2008 16:32:46   t13ae27
      *AAS00733: EliteClub Unternehmensaufbau
      *
      *   Rev 2.3   Dec 03 2004 15:33:44   t13ae27
      *ASL00196: Organisations-Leistungs-Bonus 2004
      *
      *   Rev 2.2   Mar 11 2004 11:35:18   t13ae27
      *Scharfschaltung
      *
      *   Rev 2.1   17 May 2001 10:56:20   t13ae30
      *AAS00274 - PIN für VB-Portal
      *
      *   Rev 2.0   22 Feb 2001 16:59:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0540                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS Lese-OZP ZWVB0082                    *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    15.05.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00274!15.05.01! 2.1! NEU                       !TH.WILLIG *
      * AAS00443!20.01.04! 2.2! neue Funktion DRVB0696    !P.Duerr   *
      * ASL00196!25.11.04! 2.3! neue Funktion DRVB0743    ! P.Duerr  *
      * AAS00733!06.03.08! 2.4! neue Funktion DRVB0895    ! P.Duerr  *
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
       01  PI-ZWVB0082.
           05  PI-ZWVB0082-FUNKTION            PIC  X(008).
               88 PI-ZWVB0082-DRVB0541              VALUE 'DRVB0541'.
200104         88 PI-ZWVB0082-DRVB0696              VALUE 'DRVB0696'.
251104         88 PI-ZWVB0082-DRVB0743              VALUE 'DRVB0743'.
060308         88 PI-ZWVB0082-DRVB0895              VALUE 'DRVB0895'.
      *
      ******************* ENDE COPY-MEMBER DRVB0541 *****************
