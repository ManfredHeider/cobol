      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0399.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0399.CPv  $
      *
      *   Rev 2.0   May 26 1999 17:14:20   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.1   Apr 09 1997 16:49:06   T13AE31
      *EINFUEHRUNG PEW
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0399                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA0399                    *
      *    --------------------------------------------------------   *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!04.08.97! 1.0! NEU                       ! D.WILHELM*
      * AFPA0012!01.09.97! 1.1! ÄNDERUNG AUF ANFORDERUNG  ! D.WILHELM*
      *         !        !    ! SWO 19.08.97              !          *
      * AFPA0012!23.10.97! 1.2! ÄNDERUNG PMPE-PG-DATEN    ! D.WILHELM*
      *         !        !    !      IN PMPE-PG-PM-DATEN  !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA0399: LESEN PMPE
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA0399.
           05  PI-DRPA0399-DATEN.
               10 PI-DRPA0399-PGEB-GES          PIC S9(004) COMP.
               10 PI-DRPA0399-DTA-ID            PIC S9(015) COMP-3.
               10 PI-DRPA0399-SATZ-ID           PIC S9(015) COMP-3.
      *
       01  PO-DRPA0399.
           05  PO-DRPA0399-RC                   PIC  X(003).
               88 PO-DRPA0399-RC-OK                        VALUE 'OK '.
               88 PO-DRPA0399-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRPA0399-DATEN.
               10 PO-DRPA0399-PMPG-ID           PIC S9(015) COMP-3.
               10 PO-DRPA0399-PMPG-VERSION      PIC S9(004) COMP.
               10 PO-DRPA0399-PMPE-VERTR-BEGINN PIC  X(010).
               10 PO-DRPA0399-PMPE-VERTR-ABLAUF PIC  X(010).
               10 PO-DRPA0399-KNR-ZAHLWEISE     PIC S9(004) COMP.
               10 PO-DRPA0399-PMPE-PG-VBNR      PIC S9(009) COMP.
               10 PO-DRPA0399-PMPE-KUNDE-NAME   PIC  X(025).
               10 PO-DRPA0399-PMPE-KUNDE-VNAME  PIC  X(020).
               10 PO-DRPA0399-PMPE-PG-ABR-DAT   PIC  X(010).
               10 PO-DRPA0399-PMPE-PG-EINL-DAT  PIC  X(010).
               10 PO-DRPA0399-PMPE-PIN          PIC S9(009) COMP.
               10 PO-DRPA0399-PMPE-LAST-UPDATE  PIC  X(026).
               10 PO-DRPA0399-PMPE-PG-PM-DATEN  PIC  X(300).
      *
      ******* ENDE COPY-MEMBER **************************************
