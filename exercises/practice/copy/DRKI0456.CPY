      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0456.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI0456.CPv  $
      *
      *   Rev 2.4   Dec 06 2016 15:12:10   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.3   Dec 06 2016 14:58:20   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.2   Nov 28 2016 12:15:02   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.1   Nov 28 2016 11:33:36   t13ex27
      *AKI01461: Konsolidierung Datenmodell-VB-Schnittstelle-OZP
      *
      *   Rev 2.0   Feb 27 1999 12:57:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   29 Oct 1998 17:48:44   T13EX19
      *fki00010 - Neuerstellung
      *
      *   Rev 1.0   28 Jul 1998 09:55:36   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRKI0456  FUNKTION                           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: BRENNECKE                                       *
      *    DATUM:    29.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!29.07.98! 01 ! NEUERSTELLUNG             ! BRENNECKE*
      * AKI01461!01.09.16!2.1 ! Konsolidierung VB-SST     !I. Frenzel*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0456                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  DIREKTIONSAENDERUNG SCHREIBEN <<<                    *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRKI0456.
          10 PI-DRKI0456-ADMAPERSONENID     PIC  X(013).
          10 PI-DRKI0456-ADMAPERSROLLE      PIC  X(001).
      *
       01 PO-DRKI0456.
          05 PO-DRKI0456-RC                 PIC  X(003).
             88 PO-DRKI0456-RC-OK                VALUE 'OK '.
             88 PO-DRKI0456-RC-DUPLICATE-VALUE   VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************




