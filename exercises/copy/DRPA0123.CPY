      *--------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *     Datum   :  November 1995
      *     Kopf aus:  N:PROFILEKOPFCBL.DAT
      *--------------------------------------------------------------*
      *
      *--------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *--------------------------------------------------------------*
      *    $Workfile:   DRPA0123.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0123.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:27:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   Mar 06 1997 15:01:10   P13PVCS
      *Neuerstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0123  Prov.-Struk. pruefen               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Prömpeler                                       *
      *    DATUM:    27.08.1996                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY 1 !28.08.96! 01 ! NEUERSTELLUNG             ! PRÖMPELER*
      * AFPA0002!20.01.97!    ! STRUK-DAT UND VSTAT-TAB   ! SCHäFER  *
      *         !        !    ! HINZUGEFüGT.              !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA0123                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM WORKFLOW                                      *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0123.
          05 PI-DRPA0123-PZVP-VERSION           PIC S9(004) BINARY.
          05 PI-DRPA0123-PEWA-STRUK-DAT         PIC  X(010).

       01 PO-DRPA0123.
          05 PO-DRPA0123-RC                     PIC  X(003).
             88 PO-DRPA0123-RC-OK                    VALUE 'OK '.
          05 PO-DRPA0123-DATEN.
             10 PO-DRPA0123-VTAB-MAX            PIC  9(002).
             10 PO-DRPA0123-VSTAT-TABELLE.
                15 PO-DRPA0123-VSTAT-TAB       OCCURS 20
                                  INDEXED BY PO-DRPA0123-VTAB-IND.
                   20 PO-DRPA0123-VTAB-VSTAT       PIC  X(004).
                   20 PO-DRPA0123-VTAB-VSTAT-UNR   PIC S9(004) BINARY.
                   20 PO-DRPA0123-VTAB-ORT         PIC  X(030).
                   20 PO-DRPA0123-VTAB-BEREICH     PIC  X(010).
      *
      ******** ENDE COPY-MEMBER DRPA0123 *****************************
