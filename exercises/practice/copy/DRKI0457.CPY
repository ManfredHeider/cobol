      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0457.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI0457.CPv  $
      *
      *   Rev 2.8   Dec 06 2016 15:12:20   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.7   Dec 06 2016 14:58:32   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.6   Nov 28 2016 12:15:10   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.5   Nov 28 2016 11:33:46   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.4   Jun 16 2015 09:25:00   t13ex78
      *AKI01370: Konsolidierung Datenmodell für VB-SST - Programman
      *passungen
      *
      *
      *   Rev 2.3   Jun 15 2015 15:42:24   t13ex78
      *AKI01370: Konsolidierung Datenmodell für VB-SST - Programman
      *passungen
      *
      *
      *   Rev 2.2   Jun 15 2015 12:56:36   t13ex78
      *AKI01370: Konsolidierung Datenmodell für VB-SST - Programman
      *passungen
      *
      *   Rev 2.1   Jun 15 2015 12:46:10   t13ex78
      *AKI01370: Konsolidierung Datenmodell für VB-SST - Programman
      *passungen
      *
      *   Rev 2.0   Feb 27 1999 12:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Feb 1999 11:46:26   T13EX49
      *AKI00014 - Funktionen Rueck-DTA
      *
      *   Rev 1.0   28 Jul 1998 09:55:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0457  FUNKTIONEN                         *
      *                           RUECK-DTA-FUNKTIONEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    29.08.1998                                      *
      ****************************************************************
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!29.08.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      * AKI01461!17.08.16!2.5 ! Konsolidierung VB-SST     !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *  DRKI0456                                                    *
      *  - DIREKTIONSAENDERUNG SCHREIBEN                             *
      *    WIRD VON VBSCHNITTSTELLE BENUTZT                          *
      *                                                              *
      *  DRKI0458                                                    *
      *  - DIREKTIONSAENDERUNG ERMITTELN                             *
      *                                                              *
      *  DRKI0459                                                    *
      *  - KUNDENBETREUERWECHSEL ERMITTELN                           *
      *                                                              *
      *  DRKI0460                                                    *
      *  - ADRESSAENDERUNGEN ERMITTELN                               *
      *                                                              *
      *  DRKI0597                                                    *
      *  - VERTRAGSDATEN FUER ADRESSAENDERUNG ERMITTELN              *
      *                                                              *
      *  DRKI0461                                                    *
      *  - PERSONENNAMENAENDERUNG ERMITTELN                          *
      *                                                              *
      *  DRKI0613                                                    *
      *  - VERTRAGSDATEN FUER PERSONENNAMENAENDERUNGEN ERMITTELN     *
      *                                                              *
      *  DRKI0628                                                    *
      *  - VERTRAGSDATEN FUER DIREKTIONSAENDERUNG ERMITTELN          *
      *                                                              *
      *                                                              *
      ****************************************************************
       01  PI-UPKI0047.
           05 PI-UPKI0047-FUNKTION            PIC X(008).
170816        88 PI-UPKI0047-DRKI0456-DIRAEND-S   VALUE 'DRKI0456'.
              88 PI-UPKI0047-DRKI0458-DIRAEND-E   VALUE 'DRKI0458'.
              88 PI-UPKI0047-DRKI0459-KBW-E       VALUE 'DRKI0459'.
              88 PI-UPKI0047-DRKI0628-VTR-E       VALUE 'DRKI0628'.
      *
      ****** ENDE COPYMEMBER *****************************************
 
 
 
 





