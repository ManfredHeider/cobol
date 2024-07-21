      * T13EX26   Stand: 01/08/98    13:32:46
      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0385.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI0385.CPv  $
      *
      *   Rev 2.5   Jun 21 2016 07:25:36   t13ex44
      *APA02051: KÜP-Quer auf KÜP Quer ermöglichen
      *
      *   Rev 2.4   Feb 09 2011 09:17:58   t13ex44
      *FPA01213: Korrektur Ermittlung Kundenbetreuer
      *
      *   Rev 2.3   Jul 13 2010 09:51:30   t13ex44
      *APA00993: Kundenübernahme-Folgeprovision (KÜF)
      *
      *   Rev 2.2   Feb 03 2003 11:45:30   t13ex44
      *Kommissarische Kundenuebertragung
      *
      *   Rev 2.1   Oct 22 2002 10:10:16   t13ex43
      *AKI00303 : Kommissarische Kundenübertragung
      *
      *   Rev 2.0   Apr 16 1999 18:42:22   T13EX48
      *AKI00028
      *PI/PO Funktion DRKI0385
      *
      *   Rev 1.0   14 Jul 1998 17:03:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0385  FUNKTION lesen KD-Übertrag/Mitarb  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    21.07.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * apa00012!21.07.98! 01 ! NEUERSTELLUNG             ! Parent   *
      * AKI00303!21.10.02! 2.1! KDUEBERTRAGUNGSART HINZU  ! KLEEMEIER*
      * APA00993!29.04.10! 2.2! KUNDENUEBERNAHME-FOLGE    ! SCHEIDEL *
      * FPA01213!10.11.10! 2.3! KORREKTUR KDB-ERMITTLUNG  ! SCHEIDEL *
      * APA02051!31.05.16! 2.5! QUER AUF QUER ZULASSEN    ! SCHEIDEL *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI0385.
           05  PI-DRKI0385-VERTRAGS-ID        PIC X(13).
           05  PI-DRKI0385-STICHTAG           PIC X(10).
           05  PI-DRKI0385-ROLLE-KZ           PIC X(01).
 
      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI0385.
           05  PO-DRKI0385-RC                  PIC  X(03).
               88  PO-DRKI0385-RC-OK           VALUE 'OK '.
               88  PO-DRKI0385-RC-NOT-FOUND    VALUE 'NFD'.
101110         88  PO-DRKI0385-RC-NO-OLD-KDU   VALUE 'NOK'.
101110*    05 PO-DRKI0385-MITARBEITER-NR       PIC S9(09) BINARY.
101110*    05 PO-DRKI0385-KDUEBERTRAGUNGSART   PIC  X(01).
101110*    05 PO-DRKI0385-MITARBEITER-NR-ALT   PIC S9(09) BINARY.
101110     05 PO-DRKI0385-KDUALT-MITARBALT     PIC S9(009) BINARY.
101110     05 PO-DRKI0385-KDUALT-MITARBNEU     PIC S9(009) BINARY.
101110     05 PO-DRKI0385-KDUALT-UEBART        PIC  X(001).
101110     05 PO-DRKI0385-KDUNEU-MITARBALT     PIC S9(009) BINARY.
101110     05 PO-DRKI0385-KDUNEU-MITARBNEU     PIC S9(009) BINARY.
101110     05 PO-DRKI0385-KDUNEU-UEBART        PIC  X(001).
310516     05 PO-DRKI0385-GUELTIG-AB           PIC  X(010).
      *
      ******** ENDE COPY-MEMBER (DRKI0385) *******************

