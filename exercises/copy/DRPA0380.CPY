      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0380.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0380.CPv  $
      *
      *   Rev 2.3   Aug 06 2015 10:15:34   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.2   Aug 06 2015 09:17:08   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:16:52   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:26:30   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:29:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0380                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0380                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Schrieber                                        *
      *    DATUM:    02.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !21.07.97!    ! NEU                       ! SCHRIEBER*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0380
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0380.
           05 PI-DRPA0380-EINGABE.
              10 PI-DRPA0380-DTA-ID           PIC S9(15) COMP-3.
              10 PI-DRPA0380-UFKT             PIC  X(01).
       01  PO-DRPA0380.
           05 PO-DRPA0380-RC                  PIC  X(003).
              88 PO-DRPA0380-RC-OK                      VALUE 'OK '.
              88 PO-DRPA0380-RC-NOT-OK                  VALUE 'NOK'.
      *
           05 PO-DRPA0380-ERGEBNIS.
              10 PO-DRPA0380-VSTAT               PIC  X(004).
              10 PO-DRPA0380-ANZAHL-BEWEGUNG     PIC S9(009) COMP.
              10 PO-DRPA0380-SUMME-GUTSCHRIFTEN  PIC S9(009)V99 COMP-3.
              10 PO-DRPA0380-SUMME-BELASTUNGEN   PIC S9(009)V99 COMP-3.
              10 PO-DRPA0380-PG-GSMT-PROV-OZ     PIC S9(009)V99 COMP-3.
160615        10 PO-DRPA0380-PG-UST-BETRAG-GUT   PIC S9(009)V99 COMP-3.
160615        10 PO-DRPA0380-PG-UST-BETRAG-BEL   PIC S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************
 

