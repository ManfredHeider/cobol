      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1908.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA1908.CPv  $
      *
      *   Rev 2.1   Jul 01 2013 10:35:16   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1907 - Listen-1
      *
      *     SOURCE-NAME      :  DRMA1908
      *     DMR-NAME         :  DG-OV1908
      *
      *     AUTOR            :  WOLFF, STEFAN
      *     GENERIERUNG      :  27.10.00/13:44
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
000010     05 DRMA1908.
000020        07 FUNKTION               PICTURE X(8).
000030        07 OPERATION              PICTURE XXX.
000040     88 KST-SELECT                VALUE 'SEL'.
000050     88 KST-INSERT                VALUE 'INS'.
000060     88 KST-UPDATE                VALUE 'UPD'.
000070     88 KST-DELETE                VALUE 'DEL'.
000080     88 KST-START                 VALUE 'ST '.
000090     88 KST-BACKWARD              VALUE 'BW '.
000100     88 KST-FORWARD               VALUE 'FW '.
000110     88 KST-OPEN                  VALUE 'OPE'.
000120     88 KST-CLOSE                 VALUE 'CLO'.
000130     88 KST-FETCH                 VALUE 'FET'.
000140        07 VERSION                PICTURE X.
000150        07 VG                     PICTURE S9(4)  BINARY.
000160        07 VERARB-KZN             PICTURE X.
      *
      **************************************************************
