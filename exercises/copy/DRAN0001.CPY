      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0001.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0001.CPv  $
      *
      *   Rev 2.1   Jul 11 2018 09:30:34   t13ae34
      *FAN01134: In den Antragsdialogen wird zur Produktprüfung und
      * zur Einheitenberechnung fälschlicherweise das Tagesdatum bz
      *tum und nicht das fachliche Datum (Strukturerfassungsdatum b
      *
      *   Rev 2.0   Feb 27 1999 11:05:28   P13SP20
      *NT-Umstellung, Einführung MFE
      *
      *   Rev 1.1   23 Nov 1998 10:43:10   T13EX36
      *AOP00161 - Jahr2000-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 08:32:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0001                                     *
      *    FUNKTION   : IST DAS PRODUKT ZUM STICHTAG IM EING. GESCH? *
      *    -------------------------------------------------------   *
      *    ERSTELLT: HEIDER                                          *
      *    DATUM:    09.07.2018                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FAN01134!09.07.18!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0001                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0001.
          05 PI-DRAN0001-DATEN.
             10 PI-DRAN0001-PGEB-GES       PIC S9(004) COMP.
             10 PI-DRAN0001-PROD-PROD-SCHL PIC S9(004) COMP.
             10 PI-DRAN0001-STICHTAG       PIC  X(010).
      *
       01 PO-DRAN0001.
          05 PO-DRAN0001-RC              PIC  X(003).
             88 PO-DRAN0001-RC-OK             VALUE 'OK '.
             88 PO-DRAN0001-RC-NOK            VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************

