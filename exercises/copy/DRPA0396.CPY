      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0396.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA0396.CPv  $
      *
      *   Rev 2.2   Apr 07 2016 10:02:42   t13ae46
      *APA02220: 4 Vor- und Nachkommastellen zur Konditionsermittlung
      *
      *   Rev 2.1   Nov 21 2002 17:38:54   t13ex22
      *APA00219: PROV-MULTI hinzugefügt
      *
      *   Rev 2.0   Feb 27 1999 14:29:00   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   06 Aug 1998 15:06:44   T01JM2S
      *APAA0032 - PEW-RELEASE 2.0
      *
      *   Rev 1.0   23 Jul 1997 12:31:32   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA0396                                      *
      *    --------------------------------------------------------   *
      *    DATEN FÜR KONDITION ZUR GESAMTPROVISION                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    29.07.97                                         *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!29.07.97! 01 ! NEU                       ! WOLFF    *
      * APA00219!13.11.02! 2.1! PRKO-MIN-PROV-BTRG NEU    ! MUND     *
      * APA02220!10.03.16! 2.2! 4 VOR-UND NACHKOMMASTELLEN! WANG     *
      *         !        !    ! /PRKO-GSP-STZ             !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE DATEN FÜR DIE KONDITION ZUR       *
      *    GESAMTPROVISION.                                          *
      *                                                              *
      ****************************************************************
      *
             30 KOND-DATEN-GSP.
                35 PRKO-KOND-NR                     PIC S9(004) BINARY.
                35 KNR-KOND-REGEL-GSP               PIC S9(004) BINARY.
100316*         35 PRKO-GSP-STZ                     PIC S9(003)V999
100316*                                                 PACKED-DECIMAL.
100316          35 PRKO-GSP-STZ                     PIC S9(004)V9(4)
100316                                                  PACKED-DECIMAL.
                35 PRKO-MAX-PROV-BTRG               PIC S9(009) BINARY.
                35 PRKO-MIN-PROV-BTRG               PIC S9(009) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************

