      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0676.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0676.CPv  $
      *
      *   Rev 2.3   Jan 02 2017 11:19:02   t13ae13
      *AVB01422: Änderungen VB-Vertrag - VB23: Kündigung (SWL)
      *
      *   Rev 2.2   Jan 21 2010 08:48:48   t13ae13
      *FVB00747: Abbruch im VB23 bei Ermittlung Kündigungsfristen f
      *ür BD
      *
      *   Rev 2.1   Sep 26 2003 15:51:32   t13ex22
      *AVB00481: Ermittlung frühest möglicher Kündigungstermin
      *
      *   Rev 2.0   Feb 17 2003 10:59:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0676                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS FUNKTIONS-OZP UPVB0132               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    02.09.2003                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00481!02.09.03! 01 ! NEU                       ! MUND     *
      * FVB00747!20.01.10!2.2 ! Neuer RC                  ! Walb     *
      * AVB01422!08.12.16!2.3 ! PI erweitert              ! Walb     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01  PI-DRVB0676.
           05  PI-DRVB0676-DATEN.
               10 PI-DRVB0676-MA-ST-NR         PIC S9(009) COMP.
               10 PI-DRVB0676-KUEND-DAT        PIC  X(010).
081216         10 PI-DRVB0676-KNR-GES-TYP      PIC S9(004) COMP.
      *
       01  PO-DRVB0676.
           05  PO-DRVB0676-RC                  PIC  X(003).
               88 PO-DRVB0676-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0676-RC-BD                        VALUE 'BD '.
               88 PO-DRVB0676-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0676-RC-NOT-OK                    VALUE 'NOK'.
           05  PO-DRVB0676-DATUM               PIC  X(010).
      ******* ENDE COPY-MEMBER **************************************

