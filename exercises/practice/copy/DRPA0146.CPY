      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0146.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA0146.CPv  $
      *
      *   Rev 2.1   Nov 23 2015 16:18:28   t13ex24
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System
      *
      *   Rev 2.0   Feb 27 1999 14:28:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.3   12 Nov 1998 09:16:08   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.2   27 Aug 1997 10:04:12   T13EX21
      *FEPA0008 - Erweiterung Datumsroutine; Korrektur im UPPA0024
      *
      *   Rev 1.1   Mar 06 1997 15:02:58   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0146  FUNKTION Dauer ermitteln           *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Priggen                                         *
      *    DATUM:    02.10.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !02.10.96! 01 ! Neuerstellung             ! Priggen  *
180897* FEPA0008!18.08.97!    !- Erweiterung "Datumsdiffe-! Storch   *
      *         !        !    !  renz in Tagen""          !          *
      *         !        !    !- Unterfunktionscode ein-  !          *
      *         !        !    !  fügen                    !          *
      *         !        !    !- neue Funktionen          !          *
191197*         !19.11.97!    ! Neuen Returncode einfügen ! Storch   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0146                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0146.
180897    05 PI-DRPA0146-UFKT                PIC X(002).
180897       88 PI-DRPA0146-FORMAT-TAG           VALUE '01'.
180897       88 PI-DRPA0146-FORMAT-MON-MIT       VALUE '02'.
180897* FUNKTION ZäHLT ANGEFANGENE MONATE VOLL MIT.
180897       88 PI-DRPA0146-FORMAT-MON-OHNE      VALUE '03'.
180897* FUNKTION ZäHLT ANGEFANGENE MONATE NICHT MIT.
180897       88 PI-DRPA0146-FORMAT-JHR-MIT       VALUE '04'.
180897* FUNKTION ZäHLT ANGEFANGENE JAHRE VOLL MIT.
180897       88 PI-DRPA0146-FORMAT-JHR-OHNE      VALUE '05'.
180897* FUNKTION ZäHLT ANGEFANGENE JAHRE NICHT MIT.
          05 PI-DRPA0146-DATUM-ANF           PIC  X(010).
          05 PI-DRPA0146-DATUM-END           PIC  X(010).
180897*   05 PI-DRPA0146-DAUER-FORMAT        PIC  X(001).
 
       01 PO-DRPA0146.
          05 PO-DRPA0146-RC                  PIC  X(003).
             88 PO-DRPA0146-RC-OK                 VALUE 'OK '.
             88 PO-DRPA0146-RC-NOK                VALUE 'NOK'.
             88 PO-DRPA0146-RC-EDAT-NOK           VALUE 'EDN'.
          05 PO-DRPA0146-DATEN.
180897*      10 PO-DRPA0146-DAUER            PIC S9(004) BINARY.
180897       10 PO-DRPA0146-DAUER            PIC S9(009) BINARY.
      *
      ******** Ende Copy-Member **************************************

