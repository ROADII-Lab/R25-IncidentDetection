############
'''if prompt := st.chat_input("What is up?"):
    st.session_state.messages.append({"role": "user", "content": prompt})
    with st.chat_message("user"):
        st.markdown(prompt)

    with st.chat_message("assistant"):
        stream = agent.invoke(
            input=[
                {"role": m["role"], "content": m["content"]}
                for m in st.session_state.messages
            ]
        )    
        
        #response = st.write_stream(stream.content)
        response = st.write(stream.content)

    st.session_state.messages.append({"role": "assistant", "content": response})'''

'''with st.chat_message("assistant"):
    stream = client.chat.completions.create(
        model=st.session_state["openai_model"],
        messages=[
            {"role": m["role"], "content": m["content"]}
            for m in st.session_state.messages
        ],
        stream=True,
    )
    response = st.write_stream(stream)
st.session_state.messages.append({"role": "assistant", "content": response})''' 

############

'''result = ask_question(
            input=[
                {"role": m["role"], "content": m["content"]}
                for m in st.session_state.messages
            ]
            )'''

'''prompt = st.text_area("Ask a question") 
if st.button("Submit") and prompt: 
    result = ask_question(prompt)#['result']
    st.markdown("### Response") 
    st.write(result)'''

# Model results are in the file called 'MN_2020_imputed_tbins_5to1_metro_2025-05-20.csv'. How does the average value for 'Prob_Crash' vary by 'highway' category?
    