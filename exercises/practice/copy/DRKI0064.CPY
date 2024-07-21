      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0064.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0064.CPv  $
      *
      *   Rev 2.3   Jul 13 2010 09:51:24   t13ex44
      *APA00993: Kundenübernahme-Folgeprovision (KÜF)
      *
      *   Rev 2.2   Feb 03 2003 11:45:28   t13ex44
      *Kommissarische Kundenuebertragung
      *
      *   Rev 2.1   Oct 22 2002 10:09:46   t13ex43
      *AKI00303 : Kommissarische Kundenübertragung
      *
      *   Rev 2.0   Apr 16 1999 18:12:16   T13EX48
      *AKI00028
      *PI/PO DRKI0064
      *
      *   Rev 1.0   29 Sep 1997 11:44:18   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRKI0064  FUNKTION LESEN MITARBEITER/TERMIN  *
      *    -------------------------------------------------------   *
      *    ERSTELLT: Parent                                          *
      *    DATUM:    05.11.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * DAISY   !05.11.97! 01 ! NEUERSTELLUNG             ! Parent   *
      * apa00012!21.07.98! 1.1! pi-drki0064-av-kz entfernt! Parent   *
      * AKI00303!21.10.02! 2.1! VRBKOMMBETRKZ HINZU       ! KLEEMEIER*
      * APA00993!29.4.10 ! 2.3! KUNDENUEBERNAHME-FOLGE    ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *---------------------------------------------------------------*
      ***************************************************************
      *                 PARAMETER-INPUT                             *
      ***************************************************************
       01  PI-DRKI0064.
           05  PI-DRKI0064-VERTRAGS-ID        PIC X(13).
           05  PI-DRKI0064-ROLLE-KZ           PIC X(01).
210798*    05  PI-DRKI0064-AV-KZ              PIC X(01).

      ***************************************************************
      *                 PARAMETER-OUTPUT                            *
      ***************************************************************
       01  PO-DRKI0064.
           05  PO-DRKI0064-RC                  PIC  X(03).
               88  PO-DRKI0064-RC-OK           VALUE 'OK '.
               88  PO-DRKI0064-RC-NOT-FOUND    VALUE 'NFD'.
           05 PO-DRKI0064-MITARBEITER-NR       PIC S9(09) BINARY.
           05 PO-DRKI0064-GUELTIGKEITSDAT      PIC  X(10).
211002     05 PO-DRKI0064-VRBKOMMBETRKZ        PIC  X(01).
290410     05 PO-DRKI0064-MITARBEITER-NR-ALT   PIC S9(09) BINARY.
      *
      ******** ENDE COPY-MEMBER (DRKI0064) *******************

