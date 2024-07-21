      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.11.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0839.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRVB0839.CPv  $
      *
      *   Rev 2.5   Nov 09 2012 16:08:50   t13ae13
      *AVB01252: §34f  - ZWVB0004
      *
      *   Rev 2.4   Nov 09 2012 15:35:00   t13ae13
      *AVB01252: §34f  - ZWVB0004
      *
      *   Rev 2.3   Nov 24 2010 11:05:44   t13ex57
      *AVB01120: DVAG-Wien: Vermögensschadenhaftpflichtversicherung
      *
      *   Rev 2.2   06 Jul 2007 14:24:54   t13ae30
      *FVB00682: Falscher RC im ZWVB0004
      *
      *   Rev 2.1   24 Jan 2007 15:44:48   t13ex57
      *AAN00697: Gewerbeberechtigung bei Antragserfassung DVAG-Wien
      *
      *   Rev 2.0   17 Nov 2006 10:05:48   T13EX57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0839                                     *
      *    FUNKTION   : ERMITTLUNG DER GEWERBEBERECHTIGUNG           *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: U. BOLL                                         *
      *    DATUM:    17.11.2006                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00697!17.11.06!2.1 !NEU                        !U. BOLL   *
      * FVB00682!06.07.07!2.2 !RC-NFD ERGÄNZT             !TH.WILLIG *
231110* AVB01120!23.11.10!2.3 !VSH und WG DATEN ERGAENZT  ! U. BOLL  *
      * AVB01252!06.11.12!2.4 !34f-Dat und 34f-Afo neu    ! S. Walb  *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0839                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0839.
          05 PI-DRVB0839-DATEN.
             10 PI-DRVB0839-VB-NR               PIC S9(009) COMP.

      *
       01 PO-DRVB0839.
          05 PO-DRVB0839-RC                      PIC  X(003).
             88 PO-DRVB0839-RC-OK                     VALUE 'OK '.
             88 PO-DRVB0839-RC-NOK                    VALUE 'NOK'.
060707       88 PO-DRVB0839-RC-NFD                    VALUE 'NFD'.
          05 PO-DRVB0839-DATEN.
             10 PO-DRVB0839-34C-GEWO-DAT         PIC  X(010).
             10 PO-DRVB0839-AFO                  PIC  X(001).
             10 PO-DRVB0839-VA-DAT               PIC  X(010).
             10 PO-DRVB0839-MA-FG-DAT            PIC  X(010).
             10 PO-DRVB0839-127-BES-DAT          PIC  X(010).
             10 PO-DRVB0839-127-UNBES-DAT        PIC  X(010).
231110       10 PO-DRVB0839-MA-VSH-POLNR         PIC  X(25).
231110       10 PO-DRVB0839-MA-VSH-DAT           PIC  X(10).
231110       10 PO-DRVB0839-MA-VSH-VOM           PIC  X(10).
231110       10 PO-DRVB0839-MA-HROE-EINTR-WG     PIC  X(25).
231110       10 PO-DRVB0839-MA-WG-DAT            PIC  X(10).
231110       10 PO-DRVB0839-MA-WG-VOM            PIC  X(10).
061112       10 PO-DRVB0839-34F-GEWO-DAT         PIC  X(010).
061112       10 PO-DRVB0839-34F-AFO              PIC  X(001).
