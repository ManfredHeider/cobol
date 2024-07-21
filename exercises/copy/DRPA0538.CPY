      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0538.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0538.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 14:29:14   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   04 Aug 1998 14:11:46   T13EX28
      *APA00032 - Uebergabe Konditionsupro fuer PEW Release 2.0
      *
      *   Rev 1.0   12 Sep 1997 17:00:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0538                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0538                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOHLINGER                                        *
      *    DATUM:    21.10.1997                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0020!21.10.97!    ! NEU                       ! BOHLINGER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0538
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT-ALL  <<<
      *
      ****************************************************************
      *
       01  PI-DRPA0538.
           05  PI-DRPA0538-UFKT                PIC  X(02).
      *   KANN BELEGT WERDEN MIT 1=LIST-FIRST, 2=LIST NEXT, 3=QUIT
      *
           05  PI-DRPA0538-AUSWAHL-KRITERIEN.
               10 PI-DRPA0538-PGEB-GES         PIC S9(04) COMP.
               10 PI-DRPA0538-PROD-GRP         PIC S9(04) COMP.
               10 PI-DRPA0538-S-PROD-GRP       PIC  X(01).
               10 PI-DRPA0538-STICHTAG         PIC  X(10).
               10 PI-DRPA0538-S-STICHTAG       PIC  X(01).
      *
      *
       01  PO-DRPA0538.
           05  PO-DRPA0538-RC                 PIC  X(003).
               88 PO-DRPA0538-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0538-RC-EOF                       VALUE 'EOF'.
               88 PO-DRPA0538-RC-LEER                      VALUE 'LER'.
      *
           05  PO-DRPA0538-ERGEBNISZEILE.
               10 PO-DRPA0538-PROD-PROD-SCHL   PIC S9(004) COMP.
               10 PO-DRPA0538-PROD-BEGINN      PIC  X(010).
               10 PO-DRPA0538-PROD-ENDE        PIC  X(010).
               10 PO-DRPA0538-PROD-BEZ         PIC  X(040).
      *
      ******* ENDE COPY-MEMBER **************************************
