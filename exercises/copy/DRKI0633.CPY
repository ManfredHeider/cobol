      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0633.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI0633.CPv  $
      *
      *   Rev 2.8   Nov 28 2018 11:50:42   t13ex27
      *AKI01639: Zuordnung von Verträgen anhand ihrer Herkunft, auc
      *h bei Deutsche Bank Kunden, die mehr als einen Vornamen habe
      *
      *   Rev 2.7   Mar 23 2015 09:34:48   t13ex73
      *AKI01354: Kundenaktualisierung DBO-71 - Minderung von falsch
      *en Vertragszuordnungen
      *
      *   Rev 2.6   Nov 02 2010 18:40:56   t13ex12
      *FKI01311: Anpassung PLZ-Feld für Kundensuche
      *
      *   Rev 2.5   Sep 30 2009 14:49:52   t13ex51
      *AKI00852: KI-Host - Kundensuche
      *
      *   Rev 2.4   19 Nov 2007 09:54:08   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundenaktualisierung
      *
      *   Rev 2.3   18 Nov 2007 15:14:26   t13ex27
      *Reparatur
      *
      *   Rev 2.2   16 Oct 2007 14:24:16   t13ex27
      *AKL00664: Allfinanz: Anpassung Kundensuche
      *
      *   Rev 2.1   Oct 13 2003 11:13:02   t13ex49
      *AKI00114 - Dublettenbereinigung
      *
      *   Rev 2.0   Feb 27 1999 12:58:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   08 Feb 1999 18:08:12   T13EX47
      *Parameter fuer UPKI0054
      *
      *   Rev 1.0   25 Aug 1998 09:25:24   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    VERSION   01
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *AKI000001!23.09.98! 01 ! NEUERSTELLUNG             ! gbe      *
110703* AKI00114!11.07.03!    ! Neues Adresskonzept       !P. Pohl   *
      *AKL00664 !24.09.07!2.2 ! Mandantenfähigkeit        !I. Frenzel*
      * -------------------------------------------------------------*
      * AKI00852!31.08.09! 02 ! SUCHNAME AUF 30 STELLEN   ! HRO      *
      * -------------------------------------------------------------*
      * FKI01311!01.11.10! 02 ! PLZ auf 15 Stellen.       !v.d.Goltz *
      * -------------------------------------------------------------*
050315* AKI01354!05.03.15! 02 ! AnredeID hinzufuegen      !D. Dylong *
      * AKI01639!01.11.18!2.8 ! Gefundener Suchname in    ! IFR      *
      *         !        !    ! PO-Bereich                !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRKI0633                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRKI0633.
          05 PI-DRKI0633-PZVP-VERSION             PIC S9(004) BINARY.
          05 PI-DRKI0633-PROGRAMM                 PIC  X(008).
          05 PI-DRKI0633-UFKT                     PIC  9(002).
          05 PI-DRKI0633-SCHALTER                 PIC  9.
          05 PI-DRKI0633-DATEN.
310809      07 PI-DRKI0633-SUCHNAME               PIC X(030).
310809      07 PI-DRKI0633-SUCHVORNAME            PIC X(030).
050315      07 PI-DRKI0633-ANREDEID               PIC S9(004) COMP.
            07 PI-DRKI0633-GEBGRUENDDAT           PIC X(010).
            07 PI-DRKI0633-VB-ID                  PIC X(013).
            07 PI-DRKI0633-LANDID                 PIC X(003).
110703      07 PI-DRKI0633-ORT                    PIC X(040).
011110      07 PI-DRKI0633-PLZ                    PIC X(015).
110703      07 PI-DRKI0633-STR                    PIC X(040).
240907      07 PI-DRKI0633-MANDANTNR              PIC S9(009) COMP.
 
       01 PO-DRKI0633.
          05 PO-DRKI0633-RC                       PIC  X(003).
             88 PO-DRKI0633-RC-FND                       VALUE 'FND'.
             88 PO-DRKI0633-RC-NEW                       VALUE 'NEW'.
             88 PO-DRKI0633-RC-NOK                       VALUE 'NOK'.
          05 PO-DRKI0633-DATEN.
            07 PO-DRKI0633-PERSONENID             PIC X(013).
            07 PO-DRKI0633-GEBGRUENDDAT           PIC X(010).
            07 PO-DRKI0633-ANZVORNAME             PIC X(030).
            07 PO-DRKI0633-SUCHVORNAME            PIC X(030).
            07 PO-DRKI0633-KUNDENSTATUS           PIC X(001).
            07 PO-DRKI0633-KUNDENINTVBBEZ         PIC X(001).
 
      ***** ENDE-COPY-MEMBER *****************************************


