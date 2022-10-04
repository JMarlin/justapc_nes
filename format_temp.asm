
    ;TRY TO Format the track
    LDA #$4D ;Send command with MFM bit on
    JSR SEND_FDC_COMMAND
    LDA #$02 ;Send bytes/sector (512)
    JSR SEND_FDC_COMMAND
    LDA #$09 ;Send sectors/cyl
    JSR SEND_FDC_COMMAND
    LDA #$54 ;Send gap3
    JSR SEND_FDC_COMMAND
    LDA #$5A ;Send a 'filler byte'
    JSR SEND_FDC_COMMAND
    ;For each sector on the track:
    ;    Increment sector number
    ;    Send cylinder number
    ;    Send head number
    ;    Send sector number
    ;    Send sector size
    ;Read ST0
    ;Store ST0
    ;Read ST1
    ;Store ST1
    ;Read ST2
    ;Store ST2
    ;Read 4 more undefined result bytes
