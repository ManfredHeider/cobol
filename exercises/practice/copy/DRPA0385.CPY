      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0385.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA0385.CPv  $
      *
      *   Rev 2.3   Aug 13 2015 15:32:32   t13ae46
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: Dialog
      *e (PG11, PG12, PG14, PF11, PO25)
      *
      *   Rev 2.2   Aug 06 2015 09:17:16   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.1   Jul 16 2015 15:17:00   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System, Anpass
      *ungen für PDIF
      *
      *   Rev 2.0   Feb 27 1999 14:28:58   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:28:16   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:30:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0385                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0385                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Schrieber                                        *
      *    DATUM:    22.07.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !22.07.97!    ! NEU                       ! SCHRIEBER*
      * APA01932!16.06.15!2.01! NEU:  UMSATZSTEUERBETRAG  ! STORCH   *
      *         !05.08.15!2.01! ÄNDERUNGEN WIEDER RAUS    ! KÖPRÜCÜ  *
      *         !10.08.15!2.3 ! ÄNDERUNGEN WIEDER AKTIV   ! WANG     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0385
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0385.
           05  PI-DRPA0385-EINGABE.
               10 PI-DRPA0385-DTA-ID           PIC S9(15)  COMP-3.
               10 PI-DRPA0385-UFKT             PIC  X(01).
       01  PO-DRPA0385.
           05  PO-DRPA0385-RC                  PIC  X(003).
               88 PO-DRPA0385-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0385-RC-NOT-OK                    VALUE 'NOK'.
      *
           05  PO-DRPA0385-ERGEBNIS.
               10 PO-DRPA0385-ANZAHL-BEWEGUNG  PIC S9(009) COMP.
               10 PO-DRPA0385-PG-GSMT-PROV-OZ  PIC S9(009)V99 COMP-3.
160615*        10 PO-DRPA0385-PG-UST-BETRAG    PIC S9(009)V99 COMP-3.
100815         10 PO-DRPA0385-PG-UST-BETRAG    PIC S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


