      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0551.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRPA0551.CPv  $
      *
      *   Rev 2.7   Jun 22 2018 09:10:44   T13EX44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.6   Jun 21 2016 07:25:42   t13ex44
      *APA02051: KÜP-Quer auf KÜP Quer ermöglichen
      *
      *   Rev 2.5   Jul 13 2010 09:51:36   t13ex44
      *APA00993: Kundenübernahme-Folgeprovision (KÜF)
      *
      *   Rev 2.4   Jul 17 2006 12:28:50   t13ex44
      *APA00409: Kundenbetreuungsvereinbarung
      *
      *   Rev 2.3   Feb 03 2003 11:44:30   t13ex44
      *Kommissarische Kundenuebertragung
      *
      *   Rev 2.2   Oct 22 2002 10:11:04   t13ex43
      *AKI00303 : Kommissarische Kundenübertragung
      *
      *   Rev 2.1   May 27 1999 09:54:28   T13EX26
      *MFE-Umstellung 2
      *
      *   Rev 2.0   May 26 1999 16:57:46   T13EX26
      *MFE-Umstellung
      *
      *   Rev 1.0   01 Oct 1997 11:30:42   P13SP13
      *Initial revision.
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0551  FUNKTION                           *
      *                           "ERMITTELN KD-BETR ZUM STICHTAG"   *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: ?                                               *
      *    DATUM:    ?                                               *
      ****************************************************************
      *    VERSION   02
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                  ! NAME    *
      * -------------------------------------------------------------*
      * AKI00303!21.10.02! 2.02! KD-BETR-KZ HINZU          !KLEEMEIER*
      * APA00409!14.03.06! 2.04! AVERM HINZU               ! SCHEIDEL*
      * APA00993!29.04.10! 2.05! KUENDENUEBERNAHME-FOLGE   ! SCHEIDEL*
      * APA02051!31.05.16! 2.06! QUER AUF QUER ZULASSEN    ! SCHEIDEL*
      * APA02579!21.06.18! 2.7 ! HERKUNFT HINZU            ! SCHEIDEL*
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *         !        !     !                           !         *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
       01 PI-DRPA0551.
          05 PI-DRPA0551-PGEB-GES             PIC S9(004) BINARY.
          05 PI-DRPA0551-VNR                  PIC  X(015).
          05 PI-DRPA0551-ALT-VNR              PIC  X(015).
          05 PI-DRPA0551-PROV-TERM            PIC  X(010).
140306    05 PI-DRPA0551-AVERM                PIC S9(009) BINARY.
210618    05 PI-DRPA0551-KNR-BEW-HRKFT        PIC S9(004) BINARY.
 
      ***************************************************************
       01 PO-DRPA0551.
          05 PO-DRPA0551-RC                   PIC  X(003).
             88 PO-DRPA0551-RC-OK             VALUE 'OK '.
             88 PO-DRPA0551-RC-NOT-FOUND      VALUE 'NFD'.
             88 PO-DRPA0551-RC-KEIN-VERTRAG   VALUE 'KVT'.
          05 PO-DRPA0551-KD-BETR              PIC S9(009) BINARY.
211002    05 PO-DRPA0551-KD-BETR-KZ           PIC  X(001).
290410    05 PO-DRPA0551-KD-BETR-ABGEBEND     PIC S9(009) BINARY.
310516    05 PO-DRPA0551-GUELTIG-AB           PIC  X(010).
      *
      ******** ENDE COPY-MEMBER (DRPA0551) *******************
 

