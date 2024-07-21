      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.01.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1729.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1729.CPv  $
      *
      *   Rev 2.1   Mar 13 2013 17:41:58   t13ae34
      *APA01464: SEPA - Erstellung SEPA-Überweisungs-DTA für MA-Abr
      *echnung
      *
      *   Rev 2.0   Jan 14 2013 09:07:16   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *  SEPA PAIN 001.002.03
      ****************************************************************
      *    COPY-MEMBER: DRPA1729                                     *
      *    SATZBESCHREIBUNG SEPA PAIN 001.002.03                     *
      *    FÜR DIE MITARBEITER ABRECHNUNG                            *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01464!17.01.13!2.1 ! NEU                       ! HEIDER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    XML NAMENSRAUM
       01  XML-NAMENSRAUM                           PIC  X(200) VALUE
           'URN:ISO:STD:ISO:20022:TECH:XSD:PAIN.001.002.03'.

      *    I-FELDER
       01  I-PMTINF                                 PIC  9(009).
       01  I-CDTTRFTX                               PIC  9(009).
       01  I-CDTTRFTXINF                            PIC  9(009).

      *Achtung: Groß-Kleinschreibung! keine Großschreibung erwzingen!
      **** BLOCK A MESSAGEROOT
       01  CstmrCdtTrfInitn.

      ****    BLOCK B GROUP HEADER ELEMENTS
           05 GrpHdr.
      ****       MESSAGE ID
              10 MsgId                              PIC  X(035).
      ****       CREATION DATE TIME FORMAT YYYY-MM-DDTHH:MM:SSZ
              10 CreDtTm                            PIC  X(020).
      ****       NUMBER OF TRANSACTIONS
              10 NbOfTxs                            PIC  9(009).
      ****       CONTROLSUM
              10 CtrlSum                            PIC S9(009)V99.
      ****       INITIATING PARTY
              10 InitgPty.
      ****       NAME
                 15 Nm                              PIC  X(070).

      ****    BLOCK C PAYMENT INFORMATION ELEMENTS
           05 PmtInf.
      ****       PAYMENTINFORMATIONIDENTIFICATION
              10 PmtInfId                           PIC  X(050).
      ****       PAYMENTMETHOD -> KONSTANT TRF
              10 PmtMtd                             PIC  X(003).
      ****       NUMBEROFTRANSACTIONS
              10 NbOfTxs                            PIC  9(009).
      ****       CONTROLSUM
              10 CtrlSum                            PIC S9(009)V99.
      ****       PAYMENTTYPEINFORMATION
              10 PmtTpInf.
      ****          SERVICELEVEL
                 15 SvcLvl.
      ****             CODE -> KONSTANT SEPA
      ****             Tag ersetzen durch Cd
                    20 Zd                           PIC  X(004).
      ****       REQUESTEDEXECUTIONDATE FORMAT YYYY-MM-DD
              10 ReqdExctnDt                        PIC  X(010).
      ****       DEBTOR
              10 Dbtr.
      ****          NAME
                 15 Nm                              PIC  X(070).
      ****       DEBTORACCOUNT
              10 DbtrAcct.
      ****          IDENTIFICATION
      ****          Tag ersetzen durch Id
                 15 Yd.
      ****             IBAN
                    20 IBAN                         PIC  X(022).
      ****       DEBTORAGENT
              10 DbtrAgt.
      ****          FINANCIALINSTITUTION
                 15 FinInstnId.
      ****             BIC
                    20 BIC                          PIC  X(011).
      ****       CHARGE BEARER
              10 ChrgBr                             PIC  X(004).

      ****    BLOCK D " CREDITTRANSFERTRANSACTION INFORMATION
           05 CdtTrfTxInf OCCURS 99999 DEPENDING ON I-CDTTRFTXINF.
      ****       PAYMENTINFORMATIONIDENTIFICATION
              10 PmtId.
      ****           ENDTOENDIDENTIFICATION
                 15  EndToEndId                     PIC  X(035).
      ****       AMOUNT
              10 Amt.
      ****          INSTRUCTEDAMOUNT
      ****          Tag ersetzen durch InstdAmt Ccy="EUR"
                 15 InstdAmtsCcygqEURq              PIC S9(009)V99.
      ****       CREDITORAGENT
              10 CdtrAgt.
      ****          FINANCIALINSTITUTION
                 15 FinInstnId.
      ****             BIC
                    20 BIC                          PIC  X(011).
      ****       CREDITOR
              10 Cdtr.
      ****          NAME
                 15 Nm                              PIC  X(070).
      ****       CreditorAccount
              10 CdtrAcct.
      ****       Tag ersetzen durch Id
                 15 Xd.
      ****             IBAN
                    20 IBAN                         PIC  X(022).
      ****       Remittance Information - Verwendungszweck
              10 RmtInf.
      ****          Unstrukkturierter Text
                 15 Ustrd                           PIC  X(140).

      *
      ******** ENDE COPY-MEMBER **************************************
